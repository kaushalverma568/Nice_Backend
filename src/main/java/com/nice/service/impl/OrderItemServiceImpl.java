/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.AssetConstant;
import com.nice.dto.CategoryResponseDTO;
import com.nice.dto.OrderAddonsDTO;
import com.nice.dto.OrderExtrasDto;
import com.nice.dto.OrderItemResponseDTO;
import com.nice.dto.OrderProductAttributeValueDTO;
import com.nice.dto.OrderToppingsDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.OrdersItem;
import com.nice.model.ProductVariant;
import com.nice.repository.OrderItemRepository;
import com.nice.service.AssetService;
import com.nice.service.CategoryService;
import com.nice.service.OrderAddonsService;
import com.nice.service.OrderExtrasService;
import com.nice.service.OrderItemService;
import com.nice.service.OrderProductAttributeValueService;
import com.nice.service.OrderToppingsService;
import com.nice.service.ProductVariantService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
@Service(value = "orderItemService")
@Transactional(rollbackFor = Throwable.class)
public class OrderItemServiceImpl implements OrderItemService {

	/**
	 *
	 */
	private static final String ORDER_ITEM_NOT_FOUND = "order.item.not.found";

	private static final String ORDER_NOT_FOUND = "order.not.found";

	@Autowired
	private OrderItemRepository orderItemRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private OrderAddonsService orderAddonsService;

	@Autowired
	private OrderToppingsService orderToppingsService;

	@Autowired
	private OrderExtrasService orderExtrasService;

	@Autowired
	private OrderProductAttributeValueService orderProductAttributeValueService;

	@Autowired
	private AssetService assetService;

	@Override
	public OrdersItem getOrderItemDetails(final Long orderItemId) throws NotFoundException {
		Optional<OrdersItem> optOrderItem = orderItemRepository.findById(orderItemId);
		if (!optOrderItem.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage(ORDER_ITEM_NOT_FOUND, new Object[] { orderItemId }));
		}
		return optOrderItem.get();
	}

	@Override
	public List<OrdersItem> getOrderItemForOrderId(final Long orderId) throws NotFoundException {
		List<OrdersItem> orderItemList = orderItemRepository.findAllByOrderId(orderId);
		if (orderItemList.isEmpty()) {
			throw new NotFoundException(messageByLocaleService.getMessage(ORDER_NOT_FOUND, new Object[] { orderId }));
		}
		return orderItemList;
	}

	@Override
	public List<OrderItemResponseDTO> getOrderItemResponseDTOForOrderId(final Long orderId) throws NotFoundException, ValidationException {
		return toOrderItemResponseDto(orderItemRepository.findAllByOrderId(orderId));
	}

	@Override
	public List<OrdersItem> getOrderItemForReplacementOrderId(final Long orderId) throws NotFoundException {
		List<OrdersItem> orderItemList = orderItemRepository.findAllByOrderIdAndReplaced(orderId, true);
		if (orderItemList.isEmpty()) {
			throw new NotFoundException(messageByLocaleService.getMessage(ORDER_NOT_FOUND, new Object[] { orderId }));
		}
		return orderItemList;
	}

	@Override
	public List<OrderItemResponseDTO> toOrderItemResponseDto(final List<OrdersItem> orderItemList) throws NotFoundException, ValidationException {
		List<OrderItemResponseDTO> orderItemResponseDtoList = new ArrayList<>();
		for (OrdersItem orderItem : orderItemList) {
			orderItemResponseDtoList.add(toOrderItemResponseDto(orderItem));
		}
		return orderItemResponseDtoList;
	}

	/**
	 * @param orderItem
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private OrderItemResponseDTO toOrderItemResponseDto(final OrdersItem orderItem) throws NotFoundException, ValidationException {
		OrderItemResponseDTO orderItemResponseDTO = new OrderItemResponseDTO();
		OrdersItem orderItemDetail = orderItemRepository.findById(orderItem.getId())
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(ORDER_ITEM_NOT_FOUND, new Object[] { orderItem.getId() })));
		BeanUtils.copyProperties(orderItemDetail, orderItemResponseDTO);
		ProductVariant productVariant = productVariantService.getProductVariantDetail(orderItem.getProductVariant().getId());
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			orderItemResponseDTO.setProductName(productVariant.getProduct().getNameEnglish());
			orderItemResponseDTO.setMeasurement(productVariant.getUom().getMeasurementEnglish());
		} else {
			orderItemResponseDTO.setProductName(productVariant.getProduct().getNameArabic());
			orderItemResponseDTO.setMeasurement(productVariant.getUom().getMeasurementArabic());
		}
		orderItemResponseDTO.setProductImage(productVariant.getProduct().getImage());
		orderItemResponseDTO.setProductVariantId(productVariant.getId());
		orderItemResponseDTO.setProductImageUrl(assetService.getGeneratedUrl(productVariant.getProduct().getImage(), AssetConstant.PRODUCT_DIR));

		orderItemResponseDTO.setOrderQty(orderItem.getQuantity());
		Double totalOrderItemAmount = orderItem.getTotalAmt();
		/**
		 * Add product category
		 */
		CategoryResponseDTO category = categoryService.getCategory(productVariant.getProduct().getCategoryId());
		orderItemResponseDTO.setCategoryName(category.getName());
		/**
		 * Set other order components like addons, extras, toppings, attribute variants.
		 */
		orderItemResponseDTO.setOrderAddonsDtoList(orderAddonsService.getOrderAddonsListForOrderItem(orderItem.getId()));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItemResponseDTO.getOrderAddonsDtoList())) {
			totalOrderItemAmount += orderItemResponseDTO.getOrderAddonsDtoList().stream().collect(Collectors.summingDouble(OrderAddonsDTO::getAmount));
		}
		orderItemResponseDTO.setOrderExtraDtoList(orderExtrasService.getOrderExtrasListForOrderItem(orderItem.getId()));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItemResponseDTO.getOrderExtraDtoList())) {
			totalOrderItemAmount += orderItemResponseDTO.getOrderExtraDtoList().stream().collect(Collectors.summingDouble(OrderExtrasDto::getAmount));
		}
		orderItemResponseDTO
				.setOrderProductAttributeValueDtoList(orderProductAttributeValueService.getOrderProductAttributeValueListForOrderItem(orderItem.getId()));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItemResponseDTO.getOrderProductAttributeValueDtoList())) {
			totalOrderItemAmount += orderItemResponseDTO.getOrderProductAttributeValueDtoList().stream()
					.collect(Collectors.summingDouble(OrderProductAttributeValueDTO::getAmount));
		}
		orderItemResponseDTO.setOrderToppingsDtoList(orderToppingsService.getOrderToppingsListForOrderItem(orderItem.getId()));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItemResponseDTO.getOrderToppingsDtoList())) {
			totalOrderItemAmount += orderItemResponseDTO.getOrderToppingsDtoList().stream().collect(Collectors.summingDouble(OrderToppingsDto::getAmount));
		}
		orderItemResponseDTO.setTotalOrderItemAmount(totalOrderItemAmount);
		orderItemResponseDTO.setSku(productVariant.getSku());
		orderItemResponseDTO.setProductLabel(orderItemResponseDTO.getProductName().concat("-").concat(orderItemResponseDTO.getMeasurement()));
		return orderItemResponseDTO;
	}

}
