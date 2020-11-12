/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.DeliveryType;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.dto.StockAllocationDto;
import com.nice.dto.StockDetailsWiseQuantityDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.StockAllocationMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.OrdersItem;
import com.nice.model.ProductVariant;
import com.nice.model.StockAllocation;
import com.nice.model.StockDetails;
import com.nice.model.Task;
import com.nice.model.UserLogin;
import com.nice.repository.StockAllocationRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrderItemService;
import com.nice.service.OrdersService;
import com.nice.service.ProductVariantService;
import com.nice.service.StockAllocationService;
import com.nice.service.StockDetailsService;
import com.nice.service.TaskService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Apr-2020
 */
@Service(value = "stockAllocationService")
@Transactional(rollbackFor = Throwable.class)
public class StockAllocationServiceImpl implements StockAllocationService {

	@Autowired
	private OrdersService orderService;

	@Autowired
	private StockDetailsService stockDetailsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private StockAllocationRepository stockAllocationRepository;

	@Autowired
	private StockAllocationMapper stockAllocationMapper;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private OrderItemService orderItemService;

	@Override
	public Long allocateStock(final StockAllocationDto stockAllocationDto) throws NotFoundException, ValidationException {

		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		/**
		 * Call the basic validation method here, like validating not null, non zero qty etc.
		 */
		for (StockDetailsWiseQuantityDTO stockDetailsWiseQuantityDto : stockAllocationDto.getStockDetailsWiseQtyList()) {
			validateStockAllocation(stockDetailsWiseQuantityDto, stockAllocationDto.getOrderId());
		}

		/**
		 * For a given order, delivery/replacement cannot be done more than once, hence validating here.
		 */
		List<StockAllocation> stockAllocationList = stockAllocationRepository.findAllByOrderIdAndAllocatedFor(stockAllocationDto.getOrderId(),
				stockAllocationDto.getAllocatedFor());
		if (!stockAllocationList.isEmpty()) {
			throw new ValidationException(messageByLocaleService.getMessage("stock.already.allocated.order",
					new Object[] { stockAllocationDto.getAllocatedFor(), stockAllocationDto.getOrderId() }));
		}

		Orders orders = orderService.getOrderById(stockAllocationDto.getOrderId());
		/**
		 *
		 */
		DeliveryBoy deliveryBoy = null;
		Task task = null;
		if (DeliveryType.DELIVERY.getStatusValue().equals(orders.getDeliveryType())) {
			if (stockAllocationDto.getDeliveryBoyId() != null) {
				deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(stockAllocationDto.getDeliveryBoyId());
			}
			task = taskService.getTaskForOrderIdAndAllocatedFor(orders, stockAllocationDto.getAllocatedFor());
		}

		List<OrdersItem> orderItemList = null;
		/**
		 * If this is a replacement order then check only for the replaced items
		 */
		if (TaskTypeEnum.REPLACEMENT.name().equalsIgnoreCase(stockAllocationDto.getAllocatedFor())) {
			orderItemList = orderItemService.getOrderItemForReplacementOrderId(stockAllocationDto.getOrderId());
		} else {
			orderItemList = orderItemService.getOrderItemForOrderId(stockAllocationDto.getOrderId());
		}

		/**
		 * Validate if the quantity that is being allocated is available or not
		 */
		Map<String, Long> totalToBeAllocatedQtyPerProductVariantAndLotNoCombination = validateSkuLotNoAndAvailableQtyForProductVariant(
				stockAllocationDto.getStockDetailsWiseQtyList());

		/**
		 * Check of the qty being allocated is same as that ordered.
		 */

		checkAllocatedQtyBasedOnOrderedQty(totalToBeAllocatedQtyPerProductVariantAndLotNoCombination, orderItemList);

		/**
		 * Now that we have validated the inputs, let's create the task first for delivery person and assign that task to this
		 * stock allocation
		 *
		 */

		/**
		 * Save allocated Stock
		 */
		List<String> alreadyAddedProductVaraintAndLotNos = new ArrayList<>();
		for (StockDetailsWiseQuantityDTO stockDetailsWiseQuantityDTO : stockAllocationDto.getStockDetailsWiseQtyList()) {
			/**
			 * Skip the stockDetails with duplicate barcode
			 */
			String productVariantLotNoKey = stockDetailsWiseQuantityDTO.getProductVariantId() + "-" + stockDetailsWiseQuantityDTO.getLotNo();
			if (alreadyAddedProductVaraintAndLotNos.contains(productVariantLotNoKey)) {
				continue;
			}

			OrdersItem orderItem = orderItemService.getOrderItemDetails(stockDetailsWiseQuantityDTO.getOrderItemId());
			StockDetails stockDetails = stockDetailsService.getStockDetailsByProductVariantAndLotNo(stockDetailsWiseQuantityDTO.getProductVariantId(),
					stockDetailsWiseQuantityDTO.getLotNo());
			/**
			 * validate if the combination of productVariant and orderitemId is as per ordered in stock allocation
			 */
			if (!orderItem.getProductVariant().getId().equals(stockDetails.getProductVariant().getId())
					|| !stockDetailsWiseQuantityDTO.getProductVariantId().equals(orderItem.getProductVariant().getId())) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.product.stock.combination", null));
			}

			StockAllocation stockAllocation = stockAllocationMapper.toEntity(stockAllocationDto, userLogin.getId());
			stockAllocation.setTask(task);
			stockAllocation.setDeliveryBoy(deliveryBoy);
			stockAllocation.setOrderItem(orderItem);
			stockAllocation.setOrderId(orders.getId());
			stockAllocation.setVendorId(orders.getVendor().getId());
			/**
			 * Set the all added qty per barcodes.
			 */
			stockAllocation.setQuantity(totalToBeAllocatedQtyPerProductVariantAndLotNoCombination.get(productVariantLotNoKey));
			stockAllocation.setStockDetails(stockDetails);
			stockAllocation.setActive(true);
			stockAllocationRepository.save(stockAllocation);
			alreadyAddedProductVaraintAndLotNos.add(productVariantLotNoKey);
		}
		/**
		 * Change status of order to In Process once the stock allocation is done.
		 */
		if (TaskTypeEnum.REPLACEMENT.name().equalsIgnoreCase(stockAllocationDto.getAllocatedFor())) {
			orderService.changeStatus(OrderStatusEnum.REPLACE_WAITING_FOR_PICKUP.getStatusValue(), orders);
			return orders.getId();
		} else if (TaskTypeEnum.DELIVERY.name().equalsIgnoreCase(stockAllocationDto.getAllocatedFor())) {
			orderService.changeStatus(OrderStatusEnum.WAITING_FOR_PICKUP.getStatusValue(), orders);
			return orders.getId();
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("allocated.for.value", new Object[] { stockAllocationDto.getAllocatedFor() }));
		}

	}

	/**
	 *
	 * @param stockDetailsWiseQtyList
	 * @throws NotFoundException
	 */
	private void validateStockAllocation(final StockDetailsWiseQuantityDTO stockDetailsWiseQtyDto, final Long orderId)
			throws ValidationException, NotFoundException {
		if (stockDetailsWiseQtyDto.getOrderItemId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("order.item.not.null", null));
		} else if (stockDetailsWiseQtyDto.getProductVariantId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("productVariant.id.not.null", null));
		} else if (stockDetailsWiseQtyDto.getQuantity() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("quantity.not.null", null));
		} else if (stockDetailsWiseQtyDto.getQuantity() == 0) {
			throw new ValidationException(messageByLocaleService.getMessage("quantity.non.zero", null));
		}
		/**
		 * Check if assigned stock belogs to this store
		 */
		else if (!stockDetailsService.getStockDetailsByProductVariantAndLotNo(stockDetailsWiseQtyDto.getProductVariantId(), stockDetailsWiseQtyDto.getLotNo())
				.getVendorId().equals(orderService.getOrderById(orderId).getVendor().getId())) {
			ProductVariantResponseDTO productVariantResponseDto = productVariantService.getProductVariant(stockDetailsWiseQtyDto.getProductVariantId());
			throw new ValidationException(messageByLocaleService.getMessage("stock.not.avaliable", new Object[] { productVariantResponseDto.getProductName(),
					productVariantResponseDto.getMeasurement(), stockDetailsWiseQtyDto.getLotNo() }));

		}
	}

	/**
	 * @param totalToBeAllocatedQtyPerBarcode
	 * @param orderItemList
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private Map<Long, Long> checkAllocatedQtyBasedOnOrderedQty(final Map<String, Long> totalToBeAllocatedQtyPerBarcode, final List<OrdersItem> orderItemList)
			throws NotFoundException, ValidationException {

		/**
		 * Convert barcodeWiseAllocatedQty to productVariantWiseQty, so that it can be further validated
		 */
		String language = LocaleContextHolder.getLocale().getLanguage();

		Map<Long, Long> productVariantWiseQty = new HashMap<>();
		for (Entry<String, Long> productVariantLotWiseQty : totalToBeAllocatedQtyPerBarcode.entrySet()) {
			Long productVariantId = Long.valueOf(productVariantLotWiseQty.getKey().split("-")[0]);
			Long lotNo = Long.valueOf(productVariantLotWiseQty.getKey().split("-")[1]);

			StockDetails stockDetails = stockDetailsService.getStockDetailsByProductVariantAndLotNo(productVariantId, lotNo);
			if (productVariantWiseQty.get(stockDetails.getProductVariant().getId()) == null) {
				productVariantWiseQty.put(stockDetails.getProductVariant().getId(), productVariantLotWiseQty.getValue());
			} else {
				Long qty = productVariantWiseQty.get(stockDetails.getProductVariant().getId());
				productVariantWiseQty.put(stockDetails.getProductVariant().getId(), qty + productVariantLotWiseQty.getValue());
			}

		}

		/**
		 * Check if all required qty is assigned
		 */
		for (OrdersItem orderItem : orderItemList) {
			if ("en".equals(language)) {
				if (productVariantWiseQty.get(orderItem.getProductVariant().getId()) == null) {
					/**
					 * Product Variant details to display
					 */

					ProductVariant productVariant = productVariantService.getProductVariantDetail(orderItem.getProductVariant().getId());

					throw new ValidationException(messageByLocaleService.getMessage("wrong.product.assigned",
							new Object[] { productVariant.getProduct().getNameEnglish(), productVariant.getUom().getMeasurementEnglish() }));
				} else if (!orderItem.getQuantity().equals(productVariantWiseQty.get(orderItem.getProductVariant().getId()))) {
					throw new ValidationException(messageByLocaleService.getMessage("assigned.order.qty.mismatch", new Object[] {}));
				}
			} else {
				if (productVariantWiseQty.get(orderItem.getProductVariant().getId()) == null) {
					/**
					 * Product Variant details to display
					 */

					ProductVariant productVariant = productVariantService.getProductVariantDetail(orderItem.getProductVariant().getId());

					throw new ValidationException(messageByLocaleService.getMessage("wrong.product.assigned",
							new Object[] { productVariant.getProduct().getNameArabic(), productVariant.getUom().getMeasurementArabic() }));
				} else if (!orderItem.getQuantity().equals(productVariantWiseQty.get(orderItem.getProductVariant().getId()))) {
					throw new ValidationException(messageByLocaleService.getMessage("assigned.order.qty.mismatch", new Object[] {}));
				}
			}

		}

		/**
		 *
		 */
		if (orderItemList.size() != productVariantWiseQty.size()) {
			throw new ValidationException(messageByLocaleService.getMessage("extra.product.assigned", null));
		}
		return productVariantWiseQty;
	}

	/**
	 * @param stockDetailsWiseQtyList
	 * @return totalQtyPerBarcodeToBeAllocated
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private Map<String, Long> validateSkuLotNoAndAvailableQtyForProductVariant(final List<StockDetailsWiseQuantityDTO> stockDetailsWiseQtyList)
			throws NotFoundException, ValidationException {

		/**
		 * Make a Map to determine total to be allocated qty per barcode
		 */
		Map<String, Long> totalToBeAllocatedQtyPerBarcode = new HashMap<>();
		for (StockDetailsWiseQuantityDTO stockDetailsWiseQuantityDTO : stockDetailsWiseQtyList) {
			String productVariantLotNoKey = stockDetailsWiseQuantityDTO.getProductVariantId() + "-" + stockDetailsWiseQuantityDTO.getLotNo();
			if (totalToBeAllocatedQtyPerBarcode.get(productVariantLotNoKey) == null) {
				totalToBeAllocatedQtyPerBarcode.put(productVariantLotNoKey, stockDetailsWiseQuantityDTO.getQuantity());
			} else {
				Long qty = totalToBeAllocatedQtyPerBarcode.get(productVariantLotNoKey);
				totalToBeAllocatedQtyPerBarcode.put(productVariantLotNoKey, qty + stockDetailsWiseQuantityDTO.getQuantity());
			}
		}

		/**
		 * Check if sufficient stock is available to allocate
		 *
		 * Can implement Threading concepts here
		 */
		for (Entry<String, Long> barcodeWiseAloocatedStock : totalToBeAllocatedQtyPerBarcode.entrySet()) {

			StockDetails stockDetails = stockDetailsService.getStockDetailsByProductVariantAndLotNo(
					Long.valueOf(barcodeWiseAloocatedStock.getKey().split("-")[0]), Long.valueOf(barcodeWiseAloocatedStock.getKey().split("-")[1]));
			if (stockDetails.getAvailable() < barcodeWiseAloocatedStock.getValue()) {
				ProductVariant productVariant = productVariantService.getProductVariantDetail(stockDetails.getProductVariant().getId());
				if ("en".equals(LocaleContextHolder.getLocale().getLanguage())) {
					throw new ValidationException(messageByLocaleService.getMessage("insufficient.stock",
							new Object[] { productVariant.getProduct().getNameEnglish().concat("-").concat(productVariant.getUom().getMeasurementEnglish()),
									barcodeWiseAloocatedStock.getValue(), stockDetails.getAvailable() }));
				} else {
					throw new ValidationException(messageByLocaleService.getMessage("insufficient.stock",
							new Object[] { productVariant.getProduct().getNameArabic().concat("-").concat(productVariant.getUom().getMeasurementArabic()),
									barcodeWiseAloocatedStock.getValue(), stockDetails.getAvailable() }));
				}

			}
		}
		return totalToBeAllocatedQtyPerBarcode;
	}

	@Override
	public List<StockAllocation> getAllocatedStockForOrder(final Long orderId, final String allocatedFor) {
		return stockAllocationRepository.findByOrderIdAndAllocatedForIgnoreCase(orderId, allocatedFor);
	}

	@Override
	public List<StockAllocation> getAllocatedStockForOrderItem(final Long orderItemId, final String allocatedFor) throws NotFoundException {
		OrdersItem orderItem = orderItemService.getOrderItemDetails(orderItemId);
		return stockAllocationRepository.findByOrderItemAndAllocatedForIgnoreCase(orderItem, allocatedFor);
	}

	@Override
	public void sendEmailOnOrderStatusChange(final Long orderId, final String status) throws NotFoundException {
		// final Notification notification = new Notification();
		// notification.setOrderId(orderId);
		// if (NotificationQueueConstants.IN_PROCESS_ORDER.equals(status)) {
		// notification.setType(NotificationQueueConstants.IN_PROCESS_ORDER);
		// }
		// jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
	}
}
