/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.CartItemStatus;
import com.nice.constant.Constant;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.PaymentMode;
import com.nice.constant.UserType;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrderStatusDto;
import com.nice.dto.OrdersResponseDTO;
import com.nice.exception.AuthorizationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderStatusHistoryMapper;
import com.nice.model.CartAddons;
import com.nice.model.CartExtras;
import com.nice.model.CartItem;
import com.nice.model.CartProductAttributeValue;
import com.nice.model.CartToppings;
import com.nice.model.City;
import com.nice.model.Customer;
import com.nice.model.CustomerAddress;
import com.nice.model.OnlineAddons;
import com.nice.model.OnlineCart;
import com.nice.model.OnlineExtras;
import com.nice.model.OnlineProductAttributeValue;
import com.nice.model.OnlineRequest;
import com.nice.model.OnlineToppings;
import com.nice.model.OrderStatusHistory;
import com.nice.model.Orders;
import com.nice.model.OrdersAddons;
import com.nice.model.OrdersExtras;
import com.nice.model.OrdersItem;
import com.nice.model.OrdersProductAttributeValue;
import com.nice.model.OrdersToppings;
import com.nice.model.Pincode;
import com.nice.model.ProductAttributeValue;
import com.nice.model.ProductVariant;
import com.nice.model.State;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.CartItemRepository;
import com.nice.repository.OnlineAddonsRepository;
import com.nice.repository.OnlineCartRepository;
import com.nice.repository.OnlineExtrasRepository;
import com.nice.repository.OnlineProductAttributeValueRepository;
import com.nice.repository.OnlineToppingsRepository;
import com.nice.repository.OrderAddonsRepository;
import com.nice.repository.OrderExtrasRepository;
import com.nice.repository.OrderItemRepository;
import com.nice.repository.OrderProductAttributeValueRepository;
import com.nice.repository.OrderStatusHistoryRepository;
import com.nice.repository.OrderToppingsRepository;
import com.nice.repository.OrdersRepository;
import com.nice.service.CartAddonsService;
import com.nice.service.CartExtrasService;
import com.nice.service.CartItemService;
import com.nice.service.CartProductAttributeValueService;
import com.nice.service.CartToppingsService;
import com.nice.service.CityService;
import com.nice.service.CustomerAddressService;
import com.nice.service.CustomerService;
import com.nice.service.OnlineService;
import com.nice.service.OrderItemService;
import com.nice.service.OrdersService;
import com.nice.service.PincodeService;
import com.nice.service.ProductAttributeValueService;
import com.nice.service.ProductVariantService;
import com.nice.service.SettingsService;
import com.nice.service.StateService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.razorpay.RazorpayException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Apr-2020
 */
@Service(value = "orderService")
@Transactional(rollbackFor = Throwable.class)
public class OrdersServiceImpl implements OrdersService {

	/**
	 *
	 */
	private static final String NOT_FOUND = "order.not.found";

	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CustomerAddressService customerAddressService;

	@Autowired
	private OrdersRepository ordersRepository;

	@Autowired
	private OrderItemRepository ordersItemRepository;

	@Autowired
	private OnlineCartRepository onlineCartRepository;

	@Autowired
	private OnlineExtrasRepository onlineExtrasRepository;

	@Autowired
	private OnlineAddonsRepository onlineAddonsRepository;

	@Autowired
	private OnlineToppingsRepository onlineToppingsRepository;
	@Autowired
	private OnlineProductAttributeValueRepository onlineProductAttributeValueRepository;

	//
	// @Autowired
	// private OnlineService onlineService;

	@Autowired
	private CustomerService customerService;

	// @Autowired
	// private DeliveryChargeService deliveryChargeService;
	//
	// @Autowired
	// private StockAllocationService stockAllocationService;

	// @Autowired
	// private InternalStockTransferService internalStockTransferService;

	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderStatusHistoryRepository orderStatusRepository;

	// @Autowired
	// private CancelReplaceEnquiryReasonRepository
	// cancelReplaceEnquiryReasonRepository;

	@Autowired
	private OrderStatusHistoryMapper orderStatusMapper;

	// @Autowired
	// private ExportCSV exportCSV;

	// @Autowired
	// private TaskService taskService;

	@Autowired
	private PincodeService pincodeService;

	@Autowired
	private CityService cityService;

	@Autowired
	private SettingsService settingsService;

	@Autowired
	private StateService stateService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private OnlineService onlineService;

	@Autowired
	private CartItemRepository cartItemRepository;

	@Autowired
	private CartAddonsService cartAddonsService;

	@Autowired
	private CartExtrasService cartExtrasService;

	@Autowired
	private CartProductAttributeValueService cartProductAttributeValueService;

	@Autowired
	private CartToppingsService cartToppingsService;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Autowired
	private OrderToppingsRepository orderToppingsRepository;

	@Autowired
	private OrderExtrasRepository orderExtrasRepository;

	@Autowired
	private OrderAddonsRepository orderAddonsRepository;

	@Autowired
	private OrderProductAttributeValueRepository orderProductAttributeValueRepository;

	@Override
	public String validateOrder(final OrderRequestDTO orderRequestDto) throws ValidationException, NotFoundException {

		Long customerId = getCustomerIdForLoginUser();
		orderRequestDto.setCustomerId(customerId);
		CustomerAddress customerAddress = customerAddressService.getAddressDetails(orderRequestDto.getShippingAddressId());
		City city = customerAddress.getCity();

		List<CartItem> cartItemList = cartItemService.getCartListBasedOnCustomer(orderRequestDto.getCustomerId());
		if (cartItemList.isEmpty()) {
			throw new ValidationException(messageByLocaleService.getMessage("order.unavailable", null));
		}
		Long vendorId = cartItemList.get(0).getProductVariant().getVendorId();
		Vendor vendor = vendorService.getVendorDetail(vendorId);

		/**
		 * Check if the vendor servicable and customer delivery belong to same city
		 */
		// if (!(vendor.getActive() &&
		// VendorStatus.ACTIVE.name().equals(vendor.getStatus()) &&
		// vendor.getIsOrderServiceEnable())) {
		// throw new
		// ValidationException(messageByLocaleService.getMessage("vendor.unavailable.for.order",
		// null));
		// } else
		if (!vendor.getCity().getId().equals(city.getId())) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.deliver.city", new Object[] { vendor.getCity().getName() }));
		}
		/**
		 * check if the products in cart are active or not active then throw error also
		 * check for the available quantity.
		 */
		for (CartItem cartItem : cartItemList) {
			ProductVariant productVariant = productVariantService.getProductVariantDetail(cartItem.getProductVariant().getId());
			if (!productVariant.getActive().booleanValue()) {
				throw new ValidationException(messageByLocaleService.getMessage("product.inactive",
						new Object[] { productVariant.getProduct().getName(), productVariant.getUom().getUomLabel() }));
			} else {
				// TODO
				// Stock check here for grocery related orders.
				if (false /* place condition check here for grocery related order */) {
					// Long availableQty =
					// stockDetailsService.getCountForVariantForStore(productVariant, store);
					// if (availableQty < cartItem.getQuantity()) {
					// throw new
					// ValidationException(messageByLocaleService.getMessage("insufficient.stock.for.product.available.qty",
					// new Object[] { productVariant.getProduct().getName() + "-" +
					// productVariant.getUom().getUomLabel(), availableQty }));
					// }
				} else if (!productVariant.getProductAvailable().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("product.not.available",
							new Object[] { productVariant.getProduct().getName() + "-" + productVariant.getUom().getUomLabel() }));
				}

			}
		}

		Double calculatedOrderAmt = validateOrderAmount(cartItemList, orderRequestDto);
		/**
		 * Validate and prepare order object
		 */
		if (orderRequestDto.getPaymentMode().equalsIgnoreCase(PaymentMode.COD.name())) {
			Orders order = createOrder(cartItemList, orderRequestDto, calculatedOrderAmt);

			/**
			 * remove items from cart
			 */
			cartItemService.deleteCartItemForCustomer(orderRequestDto.getCustomerId());

			return String.valueOf(order.getId());
		} else {
			OnlineRequest onlineRequest = new OnlineRequest();

			if (calculatedOrderAmt.equals(0D)) {
				throw new ValidationException(messageByLocaleService.getMessage("order.amt.non.zero", null));
			}
			Double orderAmt = calculatedOrderAmt * 100;
			onlineRequest.setAmount(orderAmt.intValue());
			onlineRequest.setCurrencyCode(settingsService.getSettingsDetailsByFieldName("CURRENCY").getFieldValue());
			try {
				String onlineOrderId = onlineService.generateOrder(onlineRequest);
				for (CartItem cartItem : cartItemList) {
					OnlineCart onlineCart = new OnlineCart();
					onlineCart.setCustomer(cartItem.getCustomer());
					onlineCart.setProductVariant(cartItem.getProductVariant());
					onlineCart.setQuantity(cartItem.getQuantity());

					onlineCart.setCityId(customerAddress.getCity().getId());
					onlineCart.setStateId(customerAddress.getState().getId());
					onlineCart.setPincodeId(customerAddress.getPincode().getId());
					onlineCart.setAddress(customerAddress.getStreetNo().concat(" ").concat(customerAddress.getBuildingName()).concat(" ")
							.concat(customerAddress.getLandmark()).concat(" ").concat(customerAddress.getCity().getName()).concat(" ")
							.concat(customerAddress.getPincode().getCodeValue()).concat(" ").concat(customerAddress.getState().getName()));
					onlineCart.setFirstName(customerAddress.getFirstName());
					onlineCart.setLastName(customerAddress.getLastName());
					onlineCart.setPhoneNumber(customerAddress.getPhoneNumber());

					onlineCart.setStatus(CartItemStatus.PAYMENT_WAITING.getStatusValue());
					onlineCart.setPaymentAmount(calculatedOrderAmt);
					cartItem.setOnlineOrderId(onlineOrderId);
					onlineCart.setOnlineOrderId(onlineOrderId);
					onlineCart.setActive(true);
					cartItemRepository.save(cartItem);
					onlineCart = onlineCartRepository.save(onlineCart);

					/**
					 * Create online extras list for this cart item
					 */
					List<CartExtras> cartExtrasList = cartExtrasService.getCartExtrasListForCartItem(cartItem.getId());
					List<OnlineExtras> onlineExtrasList = new ArrayList<>();

					for (CartExtras cartExtras : cartExtrasList) {
						OnlineExtras onlineExtras = new OnlineExtras();
						onlineExtras.setOnlineCartId(onlineCart.getId());
						onlineExtras.setProductExtras(cartExtras.getProductExtras());
						onlineExtras.setQuantity(cartExtras.getQuantity());
						onlineExtras.setActive(true);
						onlineExtras.setCreatedAt(new Date());
						onlineExtras.setUpdatedAt(new Date());
						onlineExtrasList.add(onlineExtras);
					}
					onlineExtrasRepository.saveAll(onlineExtrasList);

					/**
					 * Create online addons list for this cart item
					 */
					List<CartAddons> cartAddonsList = cartAddonsService.getCartAddonsListForCartItem(cartItem.getId());
					List<OnlineAddons> onlineAddonsList = new ArrayList<>();

					for (CartAddons cartAddons : cartAddonsList) {
						OnlineAddons onlineAddons = new OnlineAddons();
						onlineAddons.setOnlineCartId(onlineCart.getId());
						onlineAddons.setProductAddons(cartAddons.getProductAddons());
						onlineAddons.setQuantity(cartAddons.getQuantity());
						onlineAddons.setActive(true);
						onlineAddons.setCreatedAt(new Date());
						onlineAddons.setUpdatedAt(new Date());
						onlineAddonsList.add(onlineAddons);
					}
					onlineAddonsRepository.saveAll(onlineAddonsList);

					/**
					 * Create online toppings list for this cart item
					 */
					List<CartToppings> cartToppingsList = cartToppingsService.getCartToppingsListForCartItem(cartItem.getId());
					List<OnlineToppings> onlineToppingsList = new ArrayList<>();

					for (CartToppings cartToppings : cartToppingsList) {
						OnlineToppings onlineToppings = new OnlineToppings();
						onlineToppings.setOnlineCartId(onlineCart.getId());
						onlineToppings.setProductToppings(cartToppings.getProductToppings());
						onlineToppings.setQuantity(cartToppings.getQuantity());
						onlineToppings.setActive(true);
						onlineToppings.setCreatedAt(new Date());
						onlineToppings.setUpdatedAt(new Date());
						onlineToppingsList.add(onlineToppings);
					}
					onlineToppingsRepository.saveAll(onlineToppingsList);

					/**
					 * Create online product attribute value list for this cart item
					 */
					List<CartProductAttributeValue> cartProductAttributeValueList = cartProductAttributeValueService
							.getCartProductAttributeValueListForCartItem(cartItem.getId());
					List<OnlineProductAttributeValue> onlineProductAttributeValueList = new ArrayList<>();

					for (CartProductAttributeValue cartProductAttributeValue : cartProductAttributeValueList) {
						OnlineProductAttributeValue onlineProductAttributeValue = new OnlineProductAttributeValue();
						onlineProductAttributeValue.setOnlineCartId(onlineCart.getId());
						onlineProductAttributeValue.setProductAttributeValue(cartProductAttributeValue.getProductAttributeValue());
						onlineProductAttributeValue.setQuantity(cartProductAttributeValue.getQuantity());
						onlineProductAttributeValue.setActive(true);
						onlineProductAttributeValue.setCreatedAt(new Date());
						onlineProductAttributeValue.setUpdatedAt(new Date());
						onlineProductAttributeValueList.add(onlineProductAttributeValue);
					}
					onlineProductAttributeValueRepository.saveAll(onlineProductAttributeValueList);
				}
				return onlineOrderId;
			} catch (final RazorpayException e) {
				throw new ValidationException(e.getMessage());
			}
		}
	}

	@Override
	public Orders getOrderById(final Long orderId) throws NotFoundException {
		Optional<Orders> optOrder = ordersRepository.findById(orderId);
		if (!optOrder.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { orderId }));
		}
		return optOrder.get();
	}

	/**
	 * @param cartItemList
	 * @param orderRequestDto
	 * @param calculatedOrderAmt
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@Override
	public Orders createOrder(final List<CartItem> cartItemList, final OrderRequestDTO orderRequestDto, final Double calculatedOrderAmt)
			throws NotFoundException, ValidationException {
		Orders order = new Orders();
		/**
		 * Fetch Customer Details based on CustomerId
		 */
		Customer customer = customerService.getCustomerDetails(orderRequestDto.getCustomerId());
		order.setCustomer(customer);
		order.setActive(true);
		order.setPaymentMode(orderRequestDto.getPaymentMode());
		Long vendorId = cartItemList.get(0).getProductVariant().getVendorId();
		Vendor vendor = vendorService.getVendorDetail(vendorId);

		/**
		 * If order is COD order then we will get customerShipping addressId
		 */
		if (orderRequestDto.getShippingAddressId() != null) {

			CustomerAddress customerAddress = customerAddressService.getAddressDetails(orderRequestDto.getShippingAddressId());

			/**
			 * Set Address for customer in order
			 */
			order.setAddress(customerAddress.getStreetNo().concat(" ").concat(customerAddress.getBuildingName()).concat(" ")
					.concat(customerAddress.getLandmark()).concat(" ").concat(customerAddress.getCity().getName()).concat(" ")
					.concat(customerAddress.getPincode().getCodeValue()).concat(" ").concat(customerAddress.getState().getName()));
			order.setFirstName(customerAddress.getFirstName());
			order.setLastName(customerAddress.getLastName());
			order.setPhoneNumber(customerAddress.getPhoneNumber());
			order.setState(customerAddress.getState());
			order.setPincode(customerAddress.getPincode());
			order.setCity(customerAddress.getCity());
			/**
			 * Set Store based on Pincode
			 */
			order.setVendor(vendor);
		}
		/**
		 * else we will get the address details from razor pay cart with values set in
		 * orderRequestDto
		 */
		else {
			Pincode pincode = pincodeService.getPincodeDetails(orderRequestDto.getPincodeId());
			State state = stateService.getStateDetails(orderRequestDto.getStateId());
			City city = cityService.getCityDetails(orderRequestDto.getCityId());

			order.setAddress(orderRequestDto.getAddress());
			order.setFirstName(orderRequestDto.getFirstName());
			order.setLastName(orderRequestDto.getLastName());
			order.setPhoneNumber(orderRequestDto.getPhoneNumber());
			order.setState(state);
			order.setPincode(pincode);
			order.setCity(city);
			order.setVendor(vendor);

		}
		order.setOrderStatus(OrderStatusEnum.PENDING.getStatusValue());
		order.setTotalOrderAmount(calculatedOrderAmt);

		// TODO
		/**
		 * Check for respective payment gateway and implement based on same, currently
		 * is for razorpay
		 */
		/**
		 * Set Online Payment details for Order
		 */
		if (orderRequestDto.getOnlineOrderId() != null && !orderRequestDto.getOnlineOrderId().isBlank()) {
			order.setOnlineOrderId(orderRequestDto.getOnlineOrderId());
			order.setOnlinePaymentSignature(orderRequestDto.getOnlineSignature());
			order.setTransactionId(orderRequestDto.getTransactionId());
		}

		List<OrdersItem> orderItemList = new ArrayList<>();

		Double orderItemTotal = 0.0d;
		/**
		 * Order Items from cart
		 */
		for (CartItem cartItem : cartItemList) {
			OrdersItem orderItem = new OrdersItem();
			orderItem.setProductVariant(cartItem.getProductVariant());
			orderItem.setUnitPrice(cartItem.getProductVariant().getRate());
			orderItem.setUnitPriceAfterDiscount(cartItem.getProductVariant().getDiscountedRate());
			orderItem.setTotalAmt(orderItem.getUnitPriceAfterDiscount() == null ? orderItem.getUnitPrice() * cartItem.getQuantity()
					: orderItem.getUnitPriceAfterDiscount() * cartItem.getQuantity());
			orderItem.setQuantity(cartItem.getQuantity());
			if (orderItem.getUnitPriceAfterDiscount() != null) {
				orderItem.setTotalDiscountAmt(orderItem.getUnitPriceAfterDiscount() * cartItem.getQuantity());
				orderItem.setTotalDiscountAmt(orderItem.getTotalAmt() - orderItem.getTotalDiscountAmt());
			}

			orderItemTotal += orderItem.getTotalAmt();
			orderItemList.add(orderItem);

			List<CartAddons> cartAddonsList = new ArrayList<>();
			List<CartExtras> cartExtrasList = new ArrayList<>();
			List<CartToppings> cartToppingsList = new ArrayList<>();
			List<CartProductAttributeValue> cartProductAttributeValueList = new ArrayList<>();

			/**
			 * if payment mode is online then get toppings,addon,extras from online tables
			 */
			if (PaymentMode.ONLINE.name().equals(orderRequestDto.getPaymentMode())) {
				List<OnlineAddons> onlineAddonsList = onlineAddonsRepository.findAllByOnlineCartId(cartItem.getOnlineCartId());
				for (OnlineAddons onlineAddons : onlineAddonsList) {
					CartAddons cartAddons = new CartAddons();
					BeanUtils.copyProperties(onlineAddons, cartAddons);
					cartAddonsList.add(cartAddons);
				}

				List<OnlineExtras> onlineExtrasList = onlineExtrasRepository.findAllByOnlineCartId(cartItem.getOnlineCartId());
				for (OnlineExtras onlineExtras : onlineExtrasList) {
					CartExtras cartExtras = new CartExtras();
					BeanUtils.copyProperties(onlineExtras, cartExtras);
					cartExtrasList.add(cartExtras);
				}
				List<OnlineToppings> onlineToppingsList = onlineToppingsRepository.findAllByOnlineCartId(cartItem.getOnlineCartId());
				for (OnlineToppings onlineToppings : onlineToppingsList) {
					CartToppings cartToppings = new CartToppings();
					BeanUtils.copyProperties(onlineToppings, cartToppings);
					cartToppingsList.add(cartToppings);
				}

				List<OnlineProductAttributeValue> onlineProductAttributeValueList = onlineProductAttributeValueRepository
						.findAllByOnlineCartId(cartItem.getOnlineCartId());
				for (OnlineProductAttributeValue onlineProductAttributeValue : onlineProductAttributeValueList) {
					CartProductAttributeValue cartProductAttributeValue = new CartProductAttributeValue();
					BeanUtils.copyProperties(onlineProductAttributeValue, cartProductAttributeValue);
					cartProductAttributeValueList.add(cartProductAttributeValue);
				}
			} else {
				cartAddonsList = cartAddonsService.getCartAddonsListForCartItem(cartItem);
				cartExtrasList = cartExtrasService.getCartExtrasListForCartItem(cartItem);
				cartToppingsList = cartToppingsService.getCartToppingsListForCartItem(cartItem);
				cartProductAttributeValueList = cartProductAttributeValueService.getCartProductAttributeValueListForCartItem(cartItem);
			}
			/**
			 * Set addons list in order items
			 */
			List<OrdersAddons> orderAddonsList = new ArrayList<>();
			for (CartAddons cartAddons : cartAddonsList) {
				OrdersAddons orderAddons = new OrdersAddons();
				orderAddons.setActive(true);
				orderAddons.setProductAddons(cartAddons.getProductAddons());
				orderAddons.setAddonsName(cartAddons.getProductAddons().getName());
				orderAddons.setQuantity(cartAddons.getQuantity());
				orderAddons.setAmount(cartAddons.getProductAddons().getRate());
				orderAddons.setDiscountedAmount(cartAddons.getProductAddons().getDiscountedRate());
				orderAddonsList.add(orderAddons);
			}
			orderItem.setOrderAddonsList(orderAddonsList);

			/**
			 * Set extras list in order items
			 */
			List<OrdersExtras> orderExtrasList = new ArrayList<>();
			for (CartExtras cartExtras : cartExtrasList) {
				OrdersExtras orderExtras = new OrdersExtras();
				orderExtras.setActive(true);
				orderExtras.setProductExtras(cartExtras.getProductExtras());
				orderExtras.setExtrasName(cartExtras.getProductExtras().getName());
				orderExtras.setQuantity(cartExtras.getQuantity());
				orderExtras.setAmount(cartExtras.getProductExtras().getRate());
				orderExtras.setDiscountedAmount(cartExtras.getProductExtras().getDiscountedRate());
				orderExtrasList.add(orderExtras);
			}
			orderItem.setOrderExtrasList(orderExtrasList);

			/**
			 * All product attribute values
			 */
			List<OrdersProductAttributeValue> orderProductAttributeValuesList = new ArrayList<>();
			for (CartProductAttributeValue cartProductAttribute : cartProductAttributeValueList) {
				ProductAttributeValue productAttributeValue = productAttributeValueService
						.getProductAttributeValueDetail(cartProductAttribute.getProductAttributeValue().getId());
				OrdersProductAttributeValue orderProductAttributeValue = new OrdersProductAttributeValue();
				orderProductAttributeValue.setActive(true);
				orderProductAttributeValue.setProductAttributeValue(cartProductAttribute.getProductAttributeValue());
				orderProductAttributeValue.setAttributeValue(productAttributeValue.getProductAttribute().getName());
				orderProductAttributeValue.setQuantity(cartProductAttribute.getQuantity());
				orderProductAttributeValue.setAmount(cartProductAttribute.getProductAttributeValue().getRate());
				orderProductAttributeValue.setDiscountedAmount(cartProductAttribute.getProductAttributeValue().getDiscountedRate());
				orderProductAttributeValuesList.add(orderProductAttributeValue);
			}
			orderItem.setOrderProductAttributeValuesList(orderProductAttributeValuesList);
			/**
			 * Set Product Toppings
			 */
			List<OrdersToppings> orderToppingsList = new ArrayList<>();
			for (CartToppings cartToppings : cartToppingsList) {
				OrdersToppings orderToppings = new OrdersToppings();
				orderToppings.setActive(true);
				orderToppings.setProductToppings(cartToppings.getProductToppings());
				orderToppings.setToppingsName(cartToppings.getProductToppings().getName());
				orderToppings.setQuantity(cartToppings.getQuantity());
				orderToppings.setAmount(cartToppings.getProductToppings().getRate());
				orderToppings.setDiscountedAmount(cartToppings.getProductToppings().getDiscountedRate());
				orderToppingsList.add(orderToppings);
			}
			orderItem.setOrderToppingsList(orderToppingsList);
		}

		// TODO
		/**
		 * Remove the coupon code related code if not required.
		 */
		/**
		 * Check for coupon code.
		 */
		// List<OrderItem> orderItemList =
		// couponService.applyDiscountOnOrderItemBasedOnCouponCode(new
		// ArrayList<>(orderItemSet), orderRequestDto.getCouponCode());

		// TODO
		/**
		 * Check if delivery charge is applicable
		 */
		// DeliveryChargeDTO deliveryCharegeDto =
		// deliveryChargeService.getDeliveryCharge(Constant.DELIVERY_CHARGE_ID);
		// if (orderItemTotal < deliveryCharegeDto.getOrdersBelow()) {
		// order.setDeliveryCharge(deliveryCharegeDto.getDeliveryChargeValue());
		// } else {
		// order.setDeliveryCharge(0d);
		// }
		order.setReplaced(false);
		ordersRepository.save(order);
		for (OrdersItem orderItem : orderItemList) {
			orderItem.setOrder(order);
			orderItem.setActive(true);
			ordersItemRepository.save(orderItem);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItem.getOrderAddonsList())) {
				for (OrdersAddons orderAddons : orderItem.getOrderAddonsList()) {
					orderAddons.setOrderItem(orderItem);
					orderAddonsRepository.save(orderAddons);
				}
			}
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItem.getOrderExtrasList())) {
				for (OrdersExtras orderExtras : orderItem.getOrderExtrasList()) {
					orderExtras.setOrderItem(orderItem);
					orderExtrasRepository.save(orderExtras);
				}
			}
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItem.getOrderToppingsList())) {
				for (OrdersToppings orderToppings : orderItem.getOrderToppingsList()) {
					orderToppings.setOrderItem(orderItem);
					orderToppingsRepository.save(orderToppings);
				}
			}
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderItem.getOrderProductAttributeValuesList())) {
				for (OrdersProductAttributeValue orderProductAttributeValues : orderItem.getOrderProductAttributeValuesList()) {
					orderProductAttributeValues.setOrderItem(orderItem);
					orderProductAttributeValueRepository.save(orderProductAttributeValues);
				}
			}
		}
		saveOrderStatusHistory(order);
		return order;
	}

	/**
	 * @param order
	 */
	private void saveOrderStatusHistory(final Orders order) {
		OrderStatusHistory orderStatus = new OrderStatusHistory();
		orderStatus.setOrderId(order.getId());
		orderStatus.setStatus(order.getOrderStatus());
		orderStatus.setActive(true);
		orderStatusRepository.save(orderStatus);
	}

	/**
	 * @param cartItemList
	 * @param orderRequestDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private Double validateOrderAmount(final List<CartItem> cartItemList, final OrderRequestDTO orderRequestDto) throws NotFoundException, ValidationException {
		Double calculatedOrderAmt = calculateTotalOrderAmt(cartItemList);

		if (!calculatedOrderAmt.equals(round(orderRequestDto.getTotalOrderAmount()))) {
			throw new ValidationException(messageByLocaleService.getMessage("order.amount.mismatch", new Object[] { calculatedOrderAmt }));
		}
		return calculatedOrderAmt;
	}

	private Double calculateTotalOrderAmt(final List<CartItem> cartItemList) throws NotFoundException {

		Double orderAmt = 0.0d;

		for (CartItem cartItem : cartItemList) {
			Double rate = cartItem.getProductVariant().getDiscountedRate() == null || cartItem.getProductVariant().getDiscountedRate() == 0.0d
					? cartItem.getProductVariant().getRate()
					: cartItem.getProductVariant().getDiscountedRate();
			/**
			 * Add the addons , extras, product attribute values, toppings amount for
			 * calculation
			 */
			List<CartAddons> cartAddonsList = cartAddonsService.getCartAddonsListForCartItem(cartItem.getId());
			Double totalAddonsAmount = 0d;
			for (CartAddons cartAddons : cartAddonsList) {
				Double addonsRate = cartAddons.getProductAddons().getDiscountedRate() == null || cartAddons.getProductAddons().getDiscountedRate() == 0.0d
						? cartAddons.getProductAddons().getRate()
						: cartAddons.getProductAddons().getDiscountedRate();
				totalAddonsAmount = addonsRate * cartAddons.getQuantity();
			}

			List<CartExtras> cartExtrasList = cartExtrasService.getCartExtrasListForCartItem(cartItem.getId());
			Double totalExtrasAmount = 0d;
			for (CartExtras cartExtras : cartExtrasList) {
				Double extrasRate = cartExtras.getProductExtras().getDiscountedRate() == null || cartExtras.getProductExtras().getDiscountedRate() == 0.0d
						? cartExtras.getProductExtras().getRate()
						: cartExtras.getProductExtras().getDiscountedRate();
				totalExtrasAmount = extrasRate * cartExtras.getQuantity();
			}

			List<CartToppings> cartToppingsList = cartToppingsService.getCartToppingsListForCartItem(cartItem.getId());
			Double totalToppingsAmount = 0d;
			for (CartToppings cartTopping : cartToppingsList) {
				Double toppingRate = cartTopping.getProductToppings().getDiscountedRate() == null
						|| cartTopping.getProductToppings().getDiscountedRate() == 0.0d ? cartTopping.getProductToppings().getRate()
								: cartTopping.getProductToppings().getDiscountedRate();
				totalToppingsAmount = toppingRate * cartTopping.getQuantity();
			}

			List<CartProductAttributeValue> cartProductAttributeList = cartProductAttributeValueService
					.getCartProductAttributeValueListForCartItem(cartItem.getId());
			Double cartProductAttributeListAmount = 0d;
			for (CartProductAttributeValue cartProductAttributeValues : cartProductAttributeList) {
				Double attributeRate = cartProductAttributeValues.getProductAttributeValue().getDiscountedRate() == null
						|| cartProductAttributeValues.getProductAttributeValue().getDiscountedRate() == 0.0d
								? cartProductAttributeValues.getProductAttributeValue().getRate()
								: cartProductAttributeValues.getProductAttributeValue().getDiscountedRate();
				cartProductAttributeListAmount = attributeRate * cartProductAttributeValues.getQuantity();
			}
			orderAmt = orderAmt + (rate * cartItem.getQuantity()) + cartProductAttributeListAmount + totalToppingsAmount + totalExtrasAmount
					+ totalAddonsAmount;
		}

		// TODO

		/**
		 * Add delivery charge if applicable
		 */
		// DeliveryChargeDTO deliveryCharegeDto =
		// deliveryChargeService.getDeliveryCharge(Constant.DELIVERY_CHARGE_ID);
		// if (orderAmt < deliveryCharegeDto.getOrdersBelow()) {
		// orderAmt = orderAmt + deliveryCharegeDto.getDeliveryChargeValue();
		// }
		return round(orderAmt);
	}

	/**
	 * @param orderAmt
	 * @return
	 */
	private Double round(final Double orderAmt) {
		Long orderAmtLong = Math.round(orderAmt * 100);
		return orderAmtLong.doubleValue() / 100;
	}

	// @Override
	// public void changeStatus(final String newStatus, final Long userId, final
	// Long deliveryBoyId, final Orders order)
	// throws NotFoundException, ValidationException {
	// if (order == null || order.getId() == 0) {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("invalid.order.change.status",
	// null));
	// }
	// String allocatedFor = TaskTypeEnum.DELIVERY.name();
	// OrderStatusEnum existingOrderStatus =
	// OrderStatusEnum.getByValue(order.getOrderStatus());
	// final String existingStockStatus = existingOrderStatus.getStockValue();
	// if (!existingOrderStatus.contains(newStatus)) {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("status.not.allowed",
	// new Object[] { newStatus,
	// order.getOrderStatus() }));
	// }
	//
	// if (newStatus.equalsIgnoreCase(Constant.IN_PROCESS)) {
	// order.setInProcessDate(new Date(System.currentTimeMillis()));
	// DeliveryBoy deliveryBoy =
	// deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
	// order.setDeliveryBoy(deliveryBoy);
	// } else if (newStatus.equalsIgnoreCase(Constant.DELIVERED)) {
	// order.setDeliveryDate(new Date(System.currentTimeMillis()));
	// } else if (newStatus.equalsIgnoreCase(Constant.REPLACE_REQUESTED)) {
	// order.setReplacementReqDate(new Date(System.currentTimeMillis()));
	// allocatedFor = TaskTypeEnum.REPLACEMENT.name();
	// } else if (newStatus.equalsIgnoreCase(Constant.REPLACE_PROCESSED)) {
	// DeliveryBoy deliveryBoy =
	// deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
	// order.setReplacementDeliveryBoy(deliveryBoy);
	// allocatedFor = TaskTypeEnum.REPLACEMENT.name();
	// } else if (newStatus.equalsIgnoreCase(Constant.REPLACED)) {
	// order.setReplacementDate(new Date(System.currentTimeMillis()));
	// allocatedFor = TaskTypeEnum.REPLACEMENT.name();
	// } else if (newStatus.equalsIgnoreCase(Constant.CANCELLED)) {
	// order.setCancelledDate(new Date(System.currentTimeMillis()));
	// } else {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("invalid.status", new
	// Object[] { newStatus }));
	// }
	// order.setOrderStatus(newStatus);
	// order.setUpdatedAt(new Date(System.currentTimeMillis()));
	// order.setUpdatedBy(userId);
	// ordersRepository.save(order);
	//
	// saveOrderStatusHistory(userId, order);
	//
	// /**
	// * Change inventory based on status
	// */
	// /**
	// * Here if the existing stock status is delivered then we dont need to
	// transfer the inventory, that will be a typical
	// * case of replacement of orders that will be handled in a different way
	// */
	// if (!Constant.DELIVERED.equalsIgnoreCase(existingStockStatus)
	// &&
	// !existingStockStatus.equalsIgnoreCase(OrderStatusEnum.getByValue(order.getOrderStatus()).getStockValue()))
	// {
	// /**
	// * Fetch list of all allocated stock based on lot and move one by one for the
	// order.
	// */
	// List<StockAllocation> stockAllocationList =
	// stockAllocationService.getAllocatedStockForOrder(order.getId(),
	// allocatedFor);
	// for (StockAllocation stockAllocation : stockAllocationList) {
	// StockTransferDto stockTransferDto = new StockTransferDto();
	// stockTransferDto.setTransferedFrom(existingStockStatus);
	// stockTransferDto.setTransferedTo(OrderStatusEnum.getByValue(order.getOrderStatus()).getStockValue());
	// stockTransferDto.setStockDetailsId(stockAllocation.getStockDetails().getId());
	// stockTransferDto.setQuantity(stockAllocation.getQuantity());
	// stockTransferDto.setOrderId(order.getId());
	// stockTransferDto.setStoreId(stockAllocation.getStoreId());
	// stockTransferDto.setOrderFrom(OrderFromEnum.WEBSITE.name());
	// internalStockTransferService.transferStock(stockTransferDto, userId);
	// }
	// }
	// /**
	// * This handles the Replacement of stock, the stock already delivered for a
	// order will be moved from delivered to
	// * replaced status
	// */
	// if (newStatus.equalsIgnoreCase(Constant.REPLACED)) {
	// List<StockAllocation> stockAllocationList =
	// stockAllocationService.getAllocatedStockForOrder(order.getId(),
	// TaskTypeEnum.REPLACEMENT.name());
	// Set<Long> orderItemIdSet = new HashSet<>();
	// for (StockAllocation stockAllocation : stockAllocationList) {
	// orderItemIdSet.add(stockAllocation.getOrderItem().getId());
	// }
	// for (Long orderItem : orderItemIdSet) {
	// List<StockAllocation> replacementStockAllocationList =
	// stockAllocationService.getAllocatedStockForOrderItem(orderItem,
	// TaskTypeEnum.DELIVERY.name());
	// for (StockAllocation stockAllocation : replacementStockAllocationList) {
	// StockTransferDto stockTransferDto = new StockTransferDto();
	// stockTransferDto.setTransferedFrom(Constant.DELIVERED);
	// stockTransferDto.setTransferedTo(Constant.REPLACED);
	// stockTransferDto.setStockDetailsId(stockAllocation.getStockDetails().getId());
	// stockTransferDto.setQuantity(stockAllocation.getQuantity());
	// stockTransferDto.setOrderId(order.getId());
	// stockTransferDto.setStoreId(stockAllocation.getStoreId());
	// stockTransferDto.setOrderFrom(OrderFromEnum.WEBSITE.name());
	// internalStockTransferService.transferStock(stockTransferDto, userId);
	// }
	// }
	// }
	// }

	/**
	 * set userId if you want to check user role also
	 */
	@Override
	public Long getOrderCountBasedOnParams(final OrderListFilterDto orderListFilterDto) throws NotFoundException {
		UserLogin userLogin = getUserLoginFromToken();
		if (userLogin.getEntityId() != null && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			orderListFilterDto.setVendorId(userLogin.getEntityId());
		}
		return ordersRepository.getOrderCountBasedOnParams(orderListFilterDto);
	}

	@Override
	public List<OrdersResponseDTO> getOrderListBasedOnParams(final Integer startIndex, final Integer pageSize, final OrderListFilterDto orderListFilterDto,
			final boolean forAdmin) throws NotFoundException {
		List<Orders> orderList = ordersRepository.getOrderListBasedOnParams(startIndex, pageSize, orderListFilterDto);
		return toDtos(orderList, !forAdmin);
	}

	private OrdersResponseDTO toDto(final Orders orders, final boolean isFromCustomer, final boolean replacementOrderItems) throws NotFoundException {
		OrdersResponseDTO orderResponseDto = new OrdersResponseDTO();
		BeanUtils.copyProperties(orders, orderResponseDto);
		/**
		 * set city field for email
		 */
		orderResponseDto.setCity(orders.getCity().getName());
		/**
		 * set pincode field for email and push notification
		 */
		orderResponseDto.setPincode(orders.getPincode().getCodeValue());

		if (isFromCustomer) {
			orderResponseDto.setCustomerName(orders.getFirstName().concat(" ").concat(orders.getLastName()));
			orderResponseDto.setPhoneNumber(orders.getCustomer().getPhoneNumber());
		} else {
			Customer customer = orders.getCustomer();
			orderResponseDto.setCustomerName(customer.getFirstName().concat(" ").concat(customer.getLastName()));
			orderResponseDto.setPhoneNumber(customer.getPhoneNumber());
		}
		orderResponseDto.setVendorName(orders.getVendor().getFirstName() + " " + orders.getVendor().getLastName());
		orderResponseDto.setVendorId(orders.getVendor().getId());
		orderResponseDto.setEmail(orders.getCustomer().getEmail());

		if (orders.getDeliveryBoy() != null) {
			orderResponseDto.setDeliveryBoyName(orders.getDeliveryBoy().getFirstName().concat(" ").concat(orders.getDeliveryBoy().getLastName()));
		}
		if (orders.getReplacementDeliveryBoy() != null) {
			orderResponseDto.setReplacementDeliveryBoyName(
					orders.getReplacementDeliveryBoy().getFirstName().concat(" ").concat(orders.getReplacementDeliveryBoy().getLastName()));
		}
		if (replacementOrderItems) {
			Long totalCountForOrder = /* setReplacementOrderItemInResponse(orders, orderResponseDto) */0l;
			orderResponseDto.setCount(totalCountForOrder);
			List<OrderStatusHistory> orderStatusList = orderStatusRepository.findAllByOrderId(orders.getId());
			orderStatusMapper.toDtos(orderStatusList);
		} else if (isFromCustomer) {
			Long totalCountForOrder = setOrderItemInResponse(orders, orderResponseDto);
			orderResponseDto.setCount(totalCountForOrder);
		} else {
			Long totalOrderQty = ordersItemRepository.getTotalItemCountForOrder(orders.getId());
			orderResponseDto.setCount(totalOrderQty);
		}
		return orderResponseDto;
	}

	/**
	 * @param orders
	 * @param orderResponse
	 * @return
	 * @throws NotFoundException
	 */
	// private Long setReplacementOrderItemInResponse(final Orders orders, final
	// OrdersResponseDTO orderResponseDto) throws
	// NotFoundException {
	// List<OrderItem> orderItemList =
	// ordersItemRepository.findAllByOrderIdAndReplaced(orders.getId(), true);
	// orderResponseDto.setOrderItemResponseDtoList(orderItemService.toOrderItemResponseDto(orderItemList));
	// Long totalCountForOrder = 0L;
	// for (OrderItem orderItem : orderItemList) {
	// totalCountForOrder += orderItem.getQuantity();
	// }
	// return totalCountForOrder;
	// }

	/**
	 * @param orders
	 * @param orderResponseDto
	 * @return
	 * @throws NotFoundException
	 */
	private Long setOrderItemInResponse(final Orders orders, final OrdersResponseDTO orderResponseDto) throws NotFoundException {
		List<OrdersItem> orderItemList = ordersItemRepository.findAllByOrderId(orders.getId());
		orderResponseDto.setOrderItemResponseDtoList(orderItemService.toOrderItemResponseDto(orderItemList));
		Long totalCountForOrder = 0L;
		for (OrdersItem orderItem : orderItemList) {
			totalCountForOrder += orderItem.getQuantity();
		}
		return totalCountForOrder;
	}

	private List<OrdersResponseDTO> toDtos(final List<Orders> orders, final boolean isFromCustomer) throws NotFoundException {
		List<OrdersResponseDTO> results = new ArrayList<>();
		for (Orders c : orders) {
			results.add(toDto(c, isFromCustomer, false));
		}
		return results;
	}

	// @Override
	// public void replaceOrder(final ReplaceCancelOrderDto replaceCancelOrderDto,
	// final Long userId) throws
	// NotFoundException, ValidationException {
	// /**
	// * Validate if order items belong to the same order
	// */
	// Orders order =
	// ordersRepository.findById(replaceCancelOrderDto.getOrderId()).orElseThrow(
	// () -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new
	// Object[] { Constant.ORDER,
	// replaceCancelOrderDto.getOrderId() })));
	// if (!Constant.DELIVERED.equals(order.getOrderStatus())) {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("order.not.delivered.already.replaced",
	// null));
	// }
	//
	// /**
	// * If the replacement request has come after 3 days of delivery throw error.
	// */
	// if
	// (CommonUtility.convetUtilDatetoLocalDate(order.getDeliveryDate()).plusDays(3).isBefore(LocalDate.now()))
	// {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("order.outside.replacement.period",
	// new Object[]
	// {}));
	// }
	//
	// for (Long orderItemId : replaceCancelOrderDto.getOrderItem()) {
	// OrderItem orderItem = orderItemService.getOrderItemDetails(orderItemId);
	// if (!orderItem.getOrder().getId().equals(order.getId())) {
	// throw new ValidationException(
	// messageByLocaleService.getMessage("invalid.order.item.for.order", new
	// Object[] { orderItem.getId(), order.getId()
	// }));
	// }
	// orderItem.setReplaced(true);
	// orderItem.setReplaceQty(orderItem.getOrderQty());
	// ordersItemRepository.save(orderItem);
	// }
	// order.setReplaceReason(replaceCancelOrderDto.getReason());
	// order.setDescription(replaceCancelOrderDto.getDescription());
	// changeStatus(Constant.REPLACE_REQUESTED, userId, null, order);
	// }

	// @Override
	// public void cancelOrder(final ReplaceCancelOrderDto replaceCancelOrderDto,
	// final Long userId) throws
	// NotFoundException, ValidationException {
	// Orders order =
	// ordersRepository.findById(replaceCancelOrderDto.getOrderId()).orElseThrow(
	// () -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new
	// Object[] { Constant.ORDER,
	// replaceCancelOrderDto.getOrderId() })));
	// if (!OrderStatusEnum.PENDING.getStatusValue().equals(order.getOrderStatus()))
	// {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("invalid.status.for.cancel",
	// null));
	// }
	// order.setCancelReason(replaceCancelOrderDto.getReason());
	// order.setDescription(replaceCancelOrderDto.getDescription());
	// changeStatus(Constant.CANCELLED, userId, null, order);
	// }

	@Override
	public OrdersResponseDTO getOrderDetails(final Long orderId, final boolean isFromAdmin) throws NotFoundException {
		Orders order = ordersRepository.findById(orderId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { orderId })));
		OrdersResponseDTO ordersResponseDTO = toDto(order, true, false);
		if (isFromAdmin) {
			Customer customer = order.getCustomer();
			ordersResponseDTO.setCustomerName(customer.getFirstName().concat(" ").concat(customer.getLastName()));
			ordersResponseDTO.setPhoneNumber(customer.getPhoneNumber());
		}
		List<OrderStatusDto> orderStatusDtoList = orderStatusMapper.toDtos(orderStatusRepository.findAllByOrderId(orderId));
		ordersResponseDTO.setOrderStatusDtoList(orderStatusDtoList);
		return ordersResponseDTO;
	}

	// @Override
	// public OrdersResponseDTO getReplacementOrderDetails(final Long orderId)
	// throws NotFoundException {
	// Orders order = ordersRepository.findById(orderId)
	// .orElseThrow(() -> new
	// NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] {
	// Constant.ORDER,
	// orderId })));
	// if (!(Constant.REPLACED.equalsIgnoreCase(order.getOrderStatus()) ||
	// Constant.REPLACE_PROCESSED.equalsIgnoreCase(order.getOrderStatus())
	// || Constant.REPLACE_REQUESTED.equalsIgnoreCase(order.getOrderStatus()))) {
	// throw new
	// NotFoundException(messageByLocaleService.getMessage("replacement.order.not.found",
	// new Object[] { orderId
	// }));
	// }
	// return toDto(order, false, true);
	// }

	// @Override
	// public List<Orders> getTodaysDeliveredOrdersForDeliveryBoy(final DeliveryBoy
	// deliveryBoy) {
	// return
	// ordersRepository.findAllByOrderStatusAndPaymentModeAndDeliveryBoyAndDeliveryDateBetween(OrderStatusEnum.DELIVERED.getStatusValue(),
	// PaymentMode.COD.name(), deliveryBoy, CommonUtility.getDateWithoutTime(new
	// Date(System.currentTimeMillis())),
	// CommonUtility.getTomorrowDateWithoutTime(new
	// Date(System.currentTimeMillis())));
	// }
	//
	// @Override
	// public Double getTotalCashCollectionByDeliveryBoyForToday(final Long
	// deliveryBoyId) {
	// return
	// ordersRepository.getTotalCashCollectionByDeliveryBoyForToday(deliveryBoyId);
	// }

	@Override
	public Optional<Orders> getOrderDetailsByOnlineOrderId(final String onlineOrderId) {
		return ordersRepository.findByOnlineOrderId(onlineOrderId);
	}

	// @Override
	// public List<String> getReasonList(final String type) {
	// return
	// cancelReplaceEnquiryReasonRepository.findAllByType(type).stream().map(CancelReplaceEnquiryReason::getReason).collect(Collectors.toList());
	// }

	// @Override
	// public void exportOrder(final OrderListFilterDto orderListFilterDto, final
	// HttpServletResponse httpServletResponse,
	// final Long userId)
	// throws NotFoundException, IOException, AuthorizationException {
	// List<String> userType = Arrays.asList(UserType.USERS.name());
	// validateUser(userId, null, userType);
	//
	// List<OrdersResponseDTO> orderResponseDtoList =
	// getOrderListBasedOnParams(null, null, orderListFilterDto, true);
	// final Object[] orderHeaderField = new Object[] { "Order Id", "Ordered On",
	// "Customer Name", "Mobile Number", "Total
	// Qty", "Order Amount",
	// "Payment Mode", "Order Status" };
	// final Object[] orderDataField = new Object[] { "id", "createdAt",
	// "customerName", "phoneNumber", "count",
	// "totalOrderAmount", "paymentMode",
	// "orderStatus" };
	// exportCSV.writeCSVFile(orderResponseDtoList, orderDataField,
	// orderHeaderField, httpServletResponse);
	// }

	@Override
	public boolean validateUser(final Long userId, final Long entityId, final List<String> userType) throws NotFoundException, AuthorizationException {
		UserLogin userLogin = getUserLoginFromToken();

		if ("SUPER_ADMIN".equalsIgnoreCase(userLogin.getRole())) {
			return true;
		}
		if (userType.contains(userLogin.getEntityType()) && (entityId == null || userLogin.getEntityId().equals(entityId))) {
			return true;
		}
		throw new AuthorizationException(messageByLocaleService.getMessage("unauthorized", null));
	}

	// @Override
	// public void deliverOrder(final Long orderId, final String taskType, final
	// Long userId) throws NotFoundException,
	// ValidationException {
	// /**
	// * validate order for status
	// */
	//
	// Optional<Orders> optOrder = ordersRepository.findById(orderId);
	// if (!optOrder.isPresent()) {
	// throw new ValidationException(messageByLocaleService.getMessage(NOT_FOUND,
	// new Object[] { orderId }));
	// }
	// if (TaskTypeEnum.DELIVERY.name().equalsIgnoreCase(taskType)) {
	// if
	// (!OrderStatusEnum.IN_PROCESS.getStatusValue().equalsIgnoreCase(optOrder.get().getOrderStatus()))
	// {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("invalid.status.for.delivery",
	// null));
	// }
	// //Task task = taskService.getTaskForOrderIdAndAllocatedFor(optOrder.get(),
	// taskType);
	// //taskService.changeTaskStatus(task.getId(),
	// TaskStatusEnum.DELIVERED.getStatusValue(), userId);
	// } else {
	// if
	// (!OrderStatusEnum.REPLACE_PROCESSED.getStatusValue().equalsIgnoreCase(optOrder.get().getOrderStatus()))
	// {
	// throw new
	// ValidationException(messageByLocaleService.getMessage("invalid.status.for.delivery",
	// null));
	// }
	// //Task task = taskService.getTaskForOrderIdAndAllocatedFor(optOrder.get(),
	// taskType);
	// //taskService.changeTaskStatus(task.getId(),
	// TaskStatusEnum.DELIVERED.getStatusValue(), userId);
	// }
	// }

	// @Override
	// public void sendEmailOnOrderStatusChange(final Long orderId, final String
	// status) throws NotFoundException {
	// Orders order = getOrderById(orderId);
	// final Notification notification = new Notification();
	// notification.setOrderId(orderId);
	// notification.setCustomerId(order.getCustomer().getId());
	// if (NotificationQueueConstants.REPLACE_ORDER_FOR_CUSTOMER.equals(status)) {
	// notification.setEmail(order.getCustomer().getEmail());
	// notification.setType(NotificationQueueConstants.REPLACE_ORDER_FOR_CUSTOMER);
	// } else if (NotificationQueueConstants.PLACE_ORDER.equals(status)) {
	// notification.setEmail(order.getCustomer().getEmail());
	// notification.setType(NotificationQueueConstants.PLACE_ORDER);
	// } else if (NotificationQueueConstants.CANCELL_ORDER_FOR_ADMIN.equals(status))
	// {
	// notification.setType(NotificationQueueConstants.CANCELL_ORDER_FOR_ADMIN);
	// } else if (NotificationQueueConstants.REPLACE_ORDER_FOR_ADMIN.equals(status))
	// {
	// notification.setType(NotificationQueueConstants.REPLACE_ORDER_FOR_ADMIN);
	// }
	// jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE,
	// notification);
	// }

	// @Override
	// public void sendSmsOnOrderStatusChange(final Long orderId, final String
	// status) throws NotFoundException {
	// Orders order = getOrderById(orderId);
	// final Notification notification = new Notification();
	// notification.setOrderId(orderId);
	// notification.setCustomerId(order.getCustomer().getId());
	// if (NotificationQueueConstants.CANCELL_ORDER_BY_ADMIN.equals(status)) {
	// notification.setType(NotificationQueueConstants.CANCELL_ORDER_BY_ADMIN);
	// } else if
	// (NotificationQueueConstants.CANCELL_ORDER_BY_CUSTOMER.equals(status)) {
	// notification.setType(NotificationQueueConstants.CANCELL_ORDER_BY_CUSTOMER);
	// }
	// jmsQueuerService.sendSms(NotificationQueueConstants.SMS_QUEUE, notification);
	// }

	// @Override
	// public void sendPushNotificationOnStatus(final Long orderId, final String
	// type) throws NotFoundException {
	// Orders order = getOrderById(orderId);
	// PushNotification pushNotification = new PushNotification();
	// if (NotificationQueueConstants.REPLACE_ORDER_FOR_DELIVERY_BOY.equals(type)) {
	// pushNotification.setDeliveryBoyId(order.getReplacementDeliveryBoy().getId());
	// } else if
	// (NotificationQueueConstants.PLACE_ORDER_FOR_DELIVERY_BOY.equals(type)) {
	// pushNotification.setDeliveryBoyId(order.getDeliveryBoy().getId());
	// } else if
	// (NotificationQueueConstants.ORDER_DELIVERY_CONFIRMATION_FOR_DELIVERY_BOY.equals(type))
	// {
	// pushNotification.setDeliveryBoyId(order.getDeliveryBoy().getId());
	// } else {
	// pushNotification.setCustomerId(order.getCustomer().getId());
	// }
	// pushNotification.setOrderId(orderId);
	// pushNotification.setType(type);
	// jmsQueuerService.sendPushNotification(NotificationQueueConstants.PUSH_NOTIFICATION_QUEUE,
	// pushNotification);
	// }

	// @Override
	// public AppPaymentDTO getDataByRazorpayOrder(final String onlineOrderId)
	// throws NotFoundException,
	// ValidationException {
	// OnlineCart onlineCart =
	// onlineCartRepository.findAllByRazorpayOrderIdAndStatus(onlineOrderId,
	// CartItemStatus.PAYMENT_WAITING.getStatusValue())
	// .get(0);
	// CompanyResponseDTO companyResponseDTO = companyService.getCompany(true);
	// AppPaymentDTO appPaymentDTO = new AppPaymentDTO();
	// appPaymentDTO.setLogoUrl(companyResponseDTO.getCompanyImage());
	// appPaymentDTO.setContactNo(onlineCart.getPhoneNumber());
	// appPaymentDTO.setCustomerFullName(onlineCart.getFirstName().concat("
	// ").concat(onlineCart.getLastName()));
	// appPaymentDTO.setEmail(onlineCart.getCustomer().getEmail());
	// appPaymentDTO.setOnlineKey(settingsService.getSettingsDetailsByNameForEncryptedFields(Constant.PAYMENT_GATEWAY_USER_NAME).getFieldValue());
	// appPaymentDTO.setRazorpayOrderId(onlineOrderId);
	// appPaymentDTO.setShippingAddress(onlineCart.getAddress());
	// return appPaymentDTO;
	// }
	private UserLogin getUserLoginFromToken() {
		Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		if (Constant.ANONYMOUS_USER.equals(principal)) {
			return null;
		}
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	private UserLogin checkForUserLogin() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (userLogin == null) {
			throw new ValidationException(messageByLocaleService.getMessage("login.first", null));
		} else {
			return userLogin;
		}
	}

	/**
	 * @throws ValidationException
	 *
	 */
	private Long getCustomerIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}
}
