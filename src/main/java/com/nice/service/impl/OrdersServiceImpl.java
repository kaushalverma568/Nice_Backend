/**
 *
 */
package com.nice.service.impl;

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.BasicStatus;
import com.nice.constant.CartItemStatus;
import com.nice.constant.Constant;
import com.nice.constant.DeliveryType;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.PaymentMethod;
import com.nice.constant.PaymentMode;
import com.nice.constant.UserType;
import com.nice.constant.VendorAccepts;
import com.nice.constant.VendorStatus;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrderStatusDto;
import com.nice.dto.OrdersResponseDTO;
import com.nice.dto.ReplaceCancelOrderDto;
import com.nice.dto.VendorResponseDTO;
import com.nice.exception.AuthorizationException;
import com.nice.exception.FileNotFoundException;
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
import com.nice.util.ExportCSV;
import com.razorpay.RazorpayException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
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

	@Autowired
	private CustomerService customerService;

	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderStatusHistoryRepository orderStatusRepository;

	@Autowired
	private OrderStatusHistoryMapper orderStatusMapper;

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

	@Autowired
	private ExportCSV exportCSV;

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
		if (!(vendor.getActive() && VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus()) && vendor.getIsOrderServiceEnable())) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.unavailable.for.order", null));
		} else if (!PaymentMethod.BOTH.getStatusValue().equalsIgnoreCase(vendor.getPaymentMethod())
				&& !orderRequestDto.getPaymentMode().equalsIgnoreCase(vendor.getPaymentMethod())) {
			throw new ValidationException(
					messageByLocaleService.getMessage("vendor.unavailable.for.mode", new Object[] { orderRequestDto.getPaymentMode().toLowerCase() }));
		} else if (!DeliveryType.BOTH.getStatusValue().equalsIgnoreCase(vendor.getDeliveryType())
				&& !orderRequestDto.getDeliveryType().equalsIgnoreCase(vendor.getDeliveryType())) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.unavailable.delivery.type", null));
		}
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			if (!vendor.getCity().getId().equals(city.getId())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.deliver.city", new Object[] { vendor.getCity().getNameEnglish() }));
			}
		} else {
			if (!vendor.getCity().getId().equals(city.getId())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.deliver.city", new Object[] { vendor.getCity().getNameArabic() }));
			}
		}

		/**
		 * check if the products in cart are active or not active then throw error also check for the available quantity.
		 */
		for (CartItem cartItem : cartItemList) {
			ProductVariant productVariant = productVariantService.getProductVariantDetail(cartItem.getProductVariant().getId());
			if (!productVariant.getActive().booleanValue()) {
				if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
					throw new ValidationException(messageByLocaleService.getMessage("product.inactive",
							new Object[] { productVariant.getProduct().getNameEnglish(), productVariant.getUom().getUomLabelEnglish() }));
				} else {
					throw new ValidationException(messageByLocaleService.getMessage("product.inactive",
							new Object[] { productVariant.getProduct().getNameArabic(), productVariant.getUom().getUomLabelArabic() }));
				}

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
					if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
						throw new ValidationException(messageByLocaleService.getMessage("product.not.available",
								new Object[] { productVariant.getProduct().getNameEnglish(), productVariant.getUom().getUomLabelEnglish() }));
					} else {
						throw new ValidationException(messageByLocaleService.getMessage("product.not.available",
								new Object[] { productVariant.getProduct().getNameArabic(), productVariant.getUom().getUomLabelArabic() }));
					}

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
			onlineRequest.setCurrencyCode("KWD");
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
					onlineCart.setAddress(makeCustomerAddress(customerAddress));
					onlineCart.setLatitude(customerAddress.getLatitude());
					onlineCart.setLongitude(customerAddress.getLongitude());
					onlineCart.setFirstName(customerAddress.getFirstName());
					onlineCart.setLastName(customerAddress.getLastName());
					onlineCart.setPhoneNumber(customerAddress.getPhoneNumber());
					onlineCart.setDeliveryType(orderRequestDto.getDeliveryType());
					onlineCart.setDescription(orderRequestDto.getDescription());
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

	private String makeCustomerAddress(final CustomerAddress customerAddress) {
		Locale locale = LocaleContextHolder.getLocale();
		if (locale.getLanguage().equals("en")) {
			return customerAddress.getStreetNo().concat(" ").concat(customerAddress.getBuildingName()).concat(" ").concat(customerAddress.getArea()).concat(" ")
					.concat(customerAddress.getCity().getNameEnglish()).concat(" ").concat(customerAddress.getPincode().getCodeValue()).concat(" ")
					.concat(customerAddress.getState().getNameEnglish());
		} else {
			return customerAddress.getStreetNo().concat(" ").concat(customerAddress.getBuildingName()).concat(" ").concat(customerAddress.getArea()).concat(" ")
					.concat(customerAddress.getCity().getNameArabic()).concat(" ").concat(customerAddress.getPincode().getCodeValue()).concat(" ")
					.concat(customerAddress.getState().getNameArabic());
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
		order.setDeliveryType(orderRequestDto.getDeliveryType());
		order.setAssignmentTryCount(0);
		order.setNotificationTimer(new Date(System.currentTimeMillis()));
		order.setDescription(orderRequestDto.getDescription());
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
			order.setAddress(makeCustomerAddress(customerAddress));
			order.setLatitude(customerAddress.getLatitude());
			order.setLongitude(customerAddress.getLongitude());
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
		 * else we will get the address details from razor pay cart with values set in orderRequestDto
		 */
		else {
			Pincode pincode = pincodeService.getPincodeDetails(orderRequestDto.getPincodeId());
			State state = stateService.getStateDetails(orderRequestDto.getStateId());
			City city = cityService.getCityDetails(orderRequestDto.getCityId());

			order.setAddress(orderRequestDto.getAddress());
			order.setLatitude(orderRequestDto.getLatitude());
			order.setLongitude(orderRequestDto.getLongitude());
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
		 * Check for respective payment gateway and implement based on same, currently is for razorpay
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
				orderAddons.setQuantity(cartAddons.getQuantity());
				orderAddons.setAmount(cartAddons.getProductAddons().getRate() * cartAddons.getQuantity());
				if (cartAddons.getProductAddons().getDiscountedRate() != null) {
					orderAddons.setDiscountedAmount(cartAddons.getProductAddons().getDiscountedRate() * cartAddons.getQuantity());
				}
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
				orderExtras.setQuantity(cartExtras.getQuantity());
				orderExtras.setAmount(cartExtras.getProductExtras().getRate() * cartExtras.getQuantity());
				if (cartExtras.getProductExtras().getDiscountedRate() != null) {
					orderExtras.setDiscountedAmount(cartExtras.getProductExtras().getDiscountedRate() * cartExtras.getQuantity());
				}

				orderExtrasList.add(orderExtras);
			}
			orderItem.setOrderExtrasList(orderExtrasList);

			/**
			 * All product attribute values
			 */
			List<OrdersProductAttributeValue> orderProductAttributeValuesList = new ArrayList<>();
			for (CartProductAttributeValue cartProductAttribute : cartProductAttributeValueList) {
				OrdersProductAttributeValue orderProductAttributeValue = new OrdersProductAttributeValue();
				orderProductAttributeValue.setActive(true);
				orderProductAttributeValue.setProductAttributeValue(cartProductAttribute.getProductAttributeValue());
				orderProductAttributeValue.setQuantity(cartProductAttribute.getQuantity());
				orderProductAttributeValue.setAmount(cartProductAttribute.getProductAttributeValue().getRate() * cartProductAttribute.getQuantity());
				if (cartProductAttribute.getProductAttributeValue().getDiscountedRate() != null) {
					orderProductAttributeValue
							.setDiscountedAmount(cartProductAttribute.getProductAttributeValue().getDiscountedRate() * cartProductAttribute.getQuantity());
				}
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
				orderToppings.setQuantity(cartToppings.getQuantity());
				orderToppings.setAmount(cartToppings.getProductToppings().getRate() * cartToppings.getQuantity());
				if (cartToppings.getProductToppings().getDiscountedRate() != null) {
					orderToppings.setDiscountedAmount(cartToppings.getProductToppings().getDiscountedRate() * cartToppings.getQuantity());
				}
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
			 * Add the addons , extras, product attribute values, toppings amount for calculation
			 */
			List<CartAddons> cartAddonsList = cartAddonsService.getCartAddonsListForCartItem(cartItem.getId());
			Double totalAddonsAmount = 0d;
			for (CartAddons cartAddons : cartAddonsList) {
				Double addonsRate = cartAddons.getProductAddons().getDiscountedRate() == null || cartAddons.getProductAddons().getDiscountedRate() == 0.0d
						? cartAddons.getProductAddons().getRate()
						: cartAddons.getProductAddons().getDiscountedRate();
				totalAddonsAmount += addonsRate * cartAddons.getQuantity();
			}

			List<CartExtras> cartExtrasList = cartExtrasService.getCartExtrasListForCartItem(cartItem.getId());
			Double totalExtrasAmount = 0d;
			for (CartExtras cartExtras : cartExtrasList) {
				Double extrasRate = cartExtras.getProductExtras().getDiscountedRate() == null || cartExtras.getProductExtras().getDiscountedRate() == 0.0d
						? cartExtras.getProductExtras().getRate()
						: cartExtras.getProductExtras().getDiscountedRate();
				totalExtrasAmount += extrasRate * cartExtras.getQuantity();
			}

			List<CartToppings> cartToppingsList = cartToppingsService.getCartToppingsListForCartItem(cartItem.getId());
			Double totalToppingsAmount = 0d;
			for (CartToppings cartTopping : cartToppingsList) {
				Double toppingRate = cartTopping.getProductToppings().getDiscountedRate() == null
						|| cartTopping.getProductToppings().getDiscountedRate() == 0.0d ? cartTopping.getProductToppings().getRate()
								: cartTopping.getProductToppings().getDiscountedRate();
				totalToppingsAmount += toppingRate * cartTopping.getQuantity();
			}

			List<CartProductAttributeValue> cartProductAttributeList = cartProductAttributeValueService
					.getCartProductAttributeValueListForCartItem(cartItem.getId());
			Double cartProductAttributeListAmount = 0d;
			for (CartProductAttributeValue cartProductAttributeValues : cartProductAttributeList) {
				Double attributeRate = cartProductAttributeValues.getProductAttributeValue().getDiscountedRate() == null
						|| cartProductAttributeValues.getProductAttributeValue().getDiscountedRate() == 0.0d
								? cartProductAttributeValues.getProductAttributeValue().getRate()
								: cartProductAttributeValues.getProductAttributeValue().getDiscountedRate();
				cartProductAttributeListAmount += attributeRate * cartProductAttributeValues.getQuantity();
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

	/**
	 * set userId if you want to check user role also
	 *
	 * @throws ValidationException
	 */
	@Override
	public Long getOrderCountBasedOnParams(final OrderListFilterDto orderListFilterDto) throws NotFoundException, ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			orderListFilterDto.setVendorId(userLogin.getEntityId());
		} else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			orderListFilterDto.setCustomerId(userLogin.getEntityId());
		}
		return ordersRepository.getOrderCountBasedOnParams(orderListFilterDto);
	}

	@Override
	public List<OrdersResponseDTO> getOrderListBasedOnParams(final Integer startIndex, final Integer pageSize, final OrderListFilterDto orderListFilterDto)
			throws NotFoundException, ValidationException {
		UserLogin userLogin = checkForUserLogin();
		boolean forAdmin = true;
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			orderListFilterDto.setVendorId(userLogin.getEntityId());
		} else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			orderListFilterDto.setCustomerId(userLogin.getEntityId());
			forAdmin = false;
		}

		List<Orders> orderList = ordersRepository.getOrderListBasedOnParams(startIndex, pageSize, orderListFilterDto);
		return toDtos(orderList, forAdmin);
	}

	private OrdersResponseDTO toDto(final Orders orders, final boolean isFromAdmin, final boolean replacementOrderItems, final boolean fullDetails)
			throws NotFoundException {
		final Locale locale = LocaleContextHolder.getLocale();
		OrdersResponseDTO orderResponseDto = new OrdersResponseDTO();
		BeanUtils.copyProperties(orders, orderResponseDto);
		/**
		 * set city field for email
		 */
		if (locale.getLanguage().equals("en")) {
			orderResponseDto.setCity(orders.getCity().getNameEnglish());
			orderResponseDto.setVendorName(orders.getVendor().getFirstNameEnglish() + " " + orders.getVendor().getLastNameEnglish());
		} else {
			orderResponseDto.setCity(orders.getCity().getNameArabic());
			orderResponseDto.setVendorName(orders.getVendor().getFirstNameArabic() + " " + orders.getVendor().getLastNameArabic());
		}
		/**
		 * set pincode field for email and push notification
		 */
		orderResponseDto.setPincode(orders.getPincode().getCodeValue());

		if (!isFromAdmin) {
			orderResponseDto.setCustomerName(orders.getFirstName().concat(" ").concat(orders.getLastName()));
			orderResponseDto.setPhoneNumber(orders.getPhoneNumber());
		} else {

			List<OrdersItem> ordersItemList = orderItemService.getOrderItemForOrderId(orders.getId());
			/**
			 * Set the total item count for the order to display it in admin panel
			 */
			Long itemCount = 0l;
			for (OrdersItem ordersItem : ordersItemList) {
				itemCount += ordersItem.getQuantity();
			}
			orderResponseDto.setItemCount(itemCount);
			Customer customer = orders.getCustomer();
			orderResponseDto.setCustomerName(customer.getFirstName().concat(" ").concat(customer.getLastName()));
			orderResponseDto.setPhoneNumber(customer.getPhoneNumber());
		}
		orderResponseDto.setVendorId(orders.getVendor().getId());
		orderResponseDto.setEmail(orders.getCustomer().getEmail());

		if (orders.getDeliveryBoy() != null) {
			orderResponseDto
					.setDeliveryBoyNameEnglish(orders.getDeliveryBoy().getFirstNameEnglish().concat(" ").concat(orders.getDeliveryBoy().getLastNameEnglish()));
			orderResponseDto
					.setDeliveryBoyNameArabic(orders.getDeliveryBoy().getFirstNameArabic().concat(" ").concat(orders.getDeliveryBoy().getLastNameArabic()));
			if (locale.getLanguage().equals("en")) {
				orderResponseDto
						.setDeliveryBoyName(orders.getDeliveryBoy().getFirstNameEnglish().concat(" ").concat(orders.getDeliveryBoy().getLastNameEnglish()));
			} else {
				orderResponseDto
						.setDeliveryBoyName(orders.getDeliveryBoy().getFirstNameArabic().concat(" ").concat(orders.getDeliveryBoy().getLastNameArabic()));
			}
		}
		if (orders.getReplacementDeliveryBoy() != null) {
			orderResponseDto.setReplacementDeliveryBoyNameEnglish(
					orders.getReplacementDeliveryBoy().getFirstNameEnglish().concat(" ").concat(orders.getReplacementDeliveryBoy().getLastNameEnglish()));
			orderResponseDto.setReplacementDeliveryBoyNameArabic(
					orders.getReplacementDeliveryBoy().getFirstNameArabic().concat(" ").concat(orders.getReplacementDeliveryBoy().getLastNameArabic()));
			if (locale.getLanguage().equals("en")) {
				orderResponseDto.setReplacementDeliveryBoyName(
						orders.getReplacementDeliveryBoy().getFirstNameEnglish().concat(" ").concat(orders.getReplacementDeliveryBoy().getLastNameEnglish()));
			} else {
				orderResponseDto.setReplacementDeliveryBoyName(
						orders.getReplacementDeliveryBoy().getFirstNameArabic().concat(" ").concat(orders.getReplacementDeliveryBoy().getLastNameArabic()));
			}
		}
		if (replacementOrderItems) {
			Long totalCountForOrder = /* setReplacementOrderItemInResponse(orders, orderResponseDto) */0l;
			orderResponseDto.setCount(totalCountForOrder);
			List<OrderStatusHistory> orderStatusList = orderStatusRepository.findAllByOrderId(orders.getId());
			orderStatusMapper.toDtos(orderStatusList);
		} else if (fullDetails) {
			Long totalCountForOrder = setOrderItemInResponse(orders, orderResponseDto);
			orderResponseDto.setCount(totalCountForOrder);
		} else {
			Long totalOrderQty = ordersItemRepository.getTotalItemCountForOrder(orders.getId());
			orderResponseDto.setCount(totalOrderQty);
		}
		VendorResponseDTO vendorDto = vendorService.getVendor(orders.getVendor().getId());
		orderResponseDto.setVendorImageUrl(vendorDto.getStoreImageUrl());
		return orderResponseDto;
	}

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

	private List<OrdersResponseDTO> toDtos(final List<Orders> orders, final boolean isFromAdmin) throws NotFoundException {
		List<OrdersResponseDTO> results = new ArrayList<>();
		for (Orders o : orders) {
			results.add(toDto(o, isFromAdmin, false, false));
		}
		return results;
	}

	@Override
	public void changeStatus(final String newStatus, final Orders order) throws NotFoundException, ValidationException {
		if (order == null || order.getId() == 0) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.order.change.status", null));
		}

		OrderStatusEnum existingOrderStatus = OrderStatusEnum.getByValue(order.getOrderStatus());
		if (!existingOrderStatus.contains(newStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("status.not.allowed", new Object[] { newStatus, order.getOrderStatus() }));
		}
		order.setOrderStatus(newStatus);
		ordersRepository.save(order);

		saveOrderStatusHistory(order);

		/**
		 * Work to be done here related to inventory for Nice; For Dussy : remove All the below stock related code.
		 */

		/**
		 * Change inventory based on status
		 */
		/**
		 * Here if the existing stock status is delivered then we dont need to transfer the inventory, that will be a typical
		 * case of replacement of orders that will be handled in a different way
		 */
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
		/**
		 * This handles the Replacement of stock, the stock already delivered for a order will be moved from delivered to
		 * replaced status
		 */
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
	}

	@Override
	public OrdersResponseDTO getOrderDetails(final Long orderId) throws NotFoundException, ValidationException {
		boolean isFromAdmin = false;
		UserLogin userLogin = checkForUserLogin();
		Long customerId = null;
		Long vendorId = null;
		/**
		 * Based on token determine the type of user.
		 */
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			customerId = userLogin.getEntityId();
		} else if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
			isFromAdmin = true;
		} else {
			isFromAdmin = true;
		}

		Orders order = ordersRepository.findById(orderId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { orderId })));
		/**
		 * If the user is Vendor or customer, check if the order actually belongs to him.
		 */
		if ((!isFromAdmin && !order.getCustomer().getId().equals(customerId))
				|| (isFromAdmin && vendorId != null && !order.getVendor().getId().equals(vendorId))) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}

		OrdersResponseDTO ordersResponseDTO = toDto(order, isFromAdmin, false, true);
		if (isFromAdmin) {
			Customer customer = order.getCustomer();
			ordersResponseDTO.setCustomerName(customer.getFirstName().concat(" ").concat(customer.getLastName()));
			ordersResponseDTO.setPhoneNumber(customer.getPhoneNumber());
		}
		List<OrderStatusDto> orderStatusDtoList = orderStatusMapper.toDtos(orderStatusRepository.findAllByOrderId(orderId));
		ordersResponseDTO.setOrderStatusDtoList(orderStatusDtoList);
		ordersResponseDTO.setOrderDate(order.getCreatedAt());
		return ordersResponseDTO;
	}

	@Override
	public Optional<Orders> getOrderDetailsByOnlineOrderId(final String onlineOrderId) {
		return ordersRepository.findByOnlineOrderId(onlineOrderId);
	}

	@Override
	public boolean validateUser(final Long userId, final Long entityId, final List<String> userType) throws NotFoundException, AuthorizationException {
		UserLogin userLogin = getUserLoginFromToken();

		if ("SUPER_ADMIN".equalsIgnoreCase(userLogin.getRole())) {
			return true;
		}
		if (userType.contains(userLogin.getEntityType()) && (entityId == null || userLogin.getEntityId().equals(entityId))) {
			return true;
		}
		throw new AuthorizationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
	}

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
	 */
	private Long getCustomerIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	@Override
	public void exportOrderList(final HttpServletResponse httpServletResponse, final OrderListFilterDto orderListFilterDto)
			throws NotFoundException, FileNotFoundException {
		List<Orders> orderList = ordersRepository.getOrderListBasedOnParams(null, null, orderListFilterDto);
		List<OrdersResponseDTO> orderDtoList = toDtos(orderList, true);
		final Object[] orderHeaderField = new Object[] { "Customer Name", "Phone Number", "Total Order Amount", "Order Status", "Payment Mode", "Vendor Name",
				"Delivery Boy Name", "Order Date" };
		final Object[] orderDataField = new Object[] { "customerName", "phoneNumber", "totalOrderAmount", "orderStatus", "paymentMode", "vendorName",
				"deliveryBoyName", "orderDate" };
		try {
			exportCSV.writeCSVFile(orderDtoList, orderDataField, orderHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}
	}

	@Override
	public void cancelOrder(final ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException {
		Orders orders = getOrderById(replaceCancelOrderDto.getOrderId());
		if (!OrderStatusEnum.PENDING.getStatusValue().equals(orders.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.status.for.cancel", null));
		}
		orders.setCancelReason(replaceCancelOrderDto.getReason());
		orders.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
		/**
		 * Add this method to cancel order.
		 */
		changeStatus(Constant.CANCELLED, orders);
	}

	@Override
	public void replaceOrder(final ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException {

		Orders orders = getOrderById(replaceCancelOrderDto.getOrderId());
		if (!Constant.DELIVERED.equals(orders.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("order.not.delivered.already.replaced", null));
		}
		/**
		 * if vendor does not accepts replacement then throw error
		 */
		if (!VendorAccepts.REPLACE.getStatusValue().equals(orders.getVendor().getAccepts())) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.not.accepts", new Object[] { orders.getVendor().getAccepts() }));
		}
		Optional<OrderStatusHistory> orderStatusHistory = orderStatusRepository.findByOrderIdAndStatus(orders.getId(),
				OrderStatusEnum.DELIVERED.getStatusValue());
		if (orderStatusHistory.isPresent()) {
			/**
			 * If the replacement request has come after a maximum days for which vendor can accepts then throw error.
			 */
			if (CommonUtility.convetUtilDatetoLocalDate(orderStatusHistory.get().getCreatedAt()).plusDays(orders.getVendor().getMaxDaysForAccept())
					.isBefore(LocalDate.now())) {
				throw new ValidationException(
						messageByLocaleService.getMessage("order.outside.replacement.period", new Object[] { orders.getVendor().getMaxDaysForAccept() }));
			}

			orders.setReturnReplaceReason(replaceCancelOrderDto.getReason());
			orders.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
			changeStatus(Constant.REPLACE_REQUESTED, orders);
		}
	}

	@Override
	public List<Orders> getAllQualifiedDeliveryOrdersForSendingNotification(final String status, final String deliveryType, final Integer assignmentTryCount,
			final Date notificationTimer) {
		return ordersRepository.findAllByOrderStatusAndDeliveryTypeAndAssignmentTryCountLessThanAndNotificationTimerLessThan(status, deliveryType,
				assignmentTryCount, notificationTimer);
	}

	@Override
	public void returnOrder(final ReplaceCancelOrderDto replaceCancelOrderDto) throws ValidationException, NotFoundException {
		Orders orders = getOrderById(replaceCancelOrderDto.getOrderId());
		if (!Constant.DELIVERED.equals(orders.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("order.not.delivered.already.returned", null));
		}
		/**
		 * if vendor does not accepts return then throw error
		 */
		if (!VendorAccepts.RETURN.getStatusValue().equals(orders.getVendor().getAccepts())) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.not.accepts", new Object[] { orders.getVendor().getAccepts() }));
		}
		Optional<OrderStatusHistory> orderStatusHistory = orderStatusRepository.findByOrderIdAndStatus(orders.getId(),
				OrderStatusEnum.DELIVERED.getStatusValue());
		if (orderStatusHistory.isPresent()) {
			/**
			 * If the return request has come after a maximum days for which vendor can accepts then throw error.
			 */
			if (CommonUtility.convetUtilDatetoLocalDate(orderStatusHistory.get().getCreatedAt()).plusDays(orders.getVendor().getMaxDaysForAccept())
					.isBefore(LocalDate.now())) {
				throw new ValidationException(
						messageByLocaleService.getMessage("order.outside.replacement.period", new Object[] { orders.getVendor().getMaxDaysForAccept() }));
			}

			orders.setReturnReplaceReason(replaceCancelOrderDto.getReason());
			orders.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
			changeStatus(Constant.RETURN_REQUESTED, orders);
		}
	}

	@Override
	public void changeStatus(final Long ordersId, final String status) throws NotFoundException, ValidationException {
		Orders orders = getOrderById(ordersId);
		changeStatus(status, orders);
		VendorResponseDTO vendorResponseDto = vendorService.getVendor(orders.getVendor().getId());

		/**
		 * If the order is a food delivery order, then if the restaurant confirms the order immediately change the state of the
		 * order to in process.
		 */
		if (Constant.CONFIRMED.equals(status) && Constant.BUSINESS_CATEGORY_FOOD_DELIVERY.equalsIgnoreCase(vendorResponseDto.getBusinessCategoryName())) {
			changeStatus(OrderStatusEnum.IN_PROCESS.getStatusValue(), orders);
		}
	}

	@Override
	public void rejectOrder(final ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException {

		Orders order = getOrderById(replaceCancelOrderDto.getOrderId());
		OrderStatusEnum existingOrderStatus = OrderStatusEnum.getByValue(order.getOrderStatus());
		if (!existingOrderStatus.contains(Constant.REJECTED)) {
			String newOrderStatusForMessage = messageByLocaleService.getMessage(Constant.REJECTED, null);
			String existingOrderStatusForMessage = messageByLocaleService.getMessage(order.getOrderStatus(), null);
			throw new ValidationException(
					messageByLocaleService.getMessage("status.not.allowed", new Object[] { newOrderStatusForMessage, existingOrderStatusForMessage }));
		}
		order.setOrderStatus(Constant.REJECTED);
		order.setCancelReason(replaceCancelOrderDto.getReason());
		order.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
		ordersRepository.save(order);
		saveOrderStatusHistory(order);
	}

	@Override
	public List<String> getNextStatus(final Long orderId) throws NotFoundException {
		Orders order = getOrderById(orderId);
		OrderStatusEnum existingOrderStatus = OrderStatusEnum.getByValue(order.getOrderStatus());
		BasicStatus<OrderStatusEnum>[] nextOrderStatus = existingOrderStatus.nextStatus();
		List<String> nextStatus = new ArrayList<>();
		for (BasicStatus<OrderStatusEnum> status : nextOrderStatus) {
			nextStatus.add(status.getStatusValue());
		}
		return nextStatus;
	}
}
