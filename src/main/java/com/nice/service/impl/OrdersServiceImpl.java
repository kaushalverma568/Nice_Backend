/**
 *
 */
package com.nice.service.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
import com.nice.constant.Role;
import com.nice.constant.SettingsConstant;
import com.nice.constant.TaskStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.constant.TicketReasonType;
import com.nice.constant.UserType;
import com.nice.constant.VendorAccepts;
import com.nice.constant.VendorStatus;
import com.nice.dto.BusinessCategoryDTO;
import com.nice.dto.CustomerResponseDTO;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrderStatusDto;
import com.nice.dto.OrdersResponseDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.dto.ReplaceCancelOrderDto;
import com.nice.dto.StockTransferDto;
import com.nice.dto.TaskDto;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.WalletTrxDTO;
import com.nice.exception.AuthorizationException;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderRatingMapper;
import com.nice.mapper.OrderStatusHistoryMapper;
import com.nice.mapper.RatingQuestionMapper;
import com.nice.model.BusinessCategory;
import com.nice.model.CartAddons;
import com.nice.model.CartExtras;
import com.nice.model.CartItem;
import com.nice.model.CartProductAttributeValue;
import com.nice.model.CartToppings;
import com.nice.model.City;
import com.nice.model.Customer;
import com.nice.model.CustomerAddress;
import com.nice.model.DeliveryBoySendNotificationHistory;
import com.nice.model.OnlineAddons;
import com.nice.model.OnlineCart;
import com.nice.model.OnlineExtras;
import com.nice.model.OnlineProductAttributeValue;
import com.nice.model.OnlineToppings;
import com.nice.model.OrderRating;
import com.nice.model.OrderStatusHistory;
import com.nice.model.Orders;
import com.nice.model.OrdersAddons;
import com.nice.model.OrdersExtras;
import com.nice.model.OrdersItem;
import com.nice.model.OrdersProductAttributeValue;
import com.nice.model.OrdersToppings;
import com.nice.model.Pincode;
import com.nice.model.ProductVariant;
import com.nice.model.RatingQuestion;
import com.nice.model.State;
import com.nice.model.StockAllocation;
import com.nice.model.Task;
import com.nice.model.TicketReason;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.CartItemRepository;
import com.nice.repository.DeliveryBoySendNotificationHistoryRepository;
import com.nice.repository.OnlineAddonsRepository;
import com.nice.repository.OnlineCartRepository;
import com.nice.repository.OnlineExtrasRepository;
import com.nice.repository.OnlineProductAttributeValueRepository;
import com.nice.repository.OnlineToppingsRepository;
import com.nice.repository.OrderAddonsRepository;
import com.nice.repository.OrderExtrasRepository;
import com.nice.repository.OrderItemRepository;
import com.nice.repository.OrderProductAttributeValueRepository;
import com.nice.repository.OrderRatingRepository;
import com.nice.repository.OrderStatusHistoryRepository;
import com.nice.repository.OrderToppingsRepository;
import com.nice.repository.OrdersRepository;
import com.nice.service.BusinessCategoryService;
import com.nice.service.CartAddonsService;
import com.nice.service.CartExtrasService;
import com.nice.service.CartItemService;
import com.nice.service.CartProductAttributeValueService;
import com.nice.service.CartToppingsService;
import com.nice.service.CityService;
import com.nice.service.CustomerAddressService;
import com.nice.service.CustomerService;
import com.nice.service.HesabePaymentService;
import com.nice.service.OrderItemService;
import com.nice.service.OrdersService;
import com.nice.service.PincodeService;
import com.nice.service.ProductVariantService;
import com.nice.service.RatingQuestionService;
import com.nice.service.StateService;
import com.nice.service.StockAllocationService;
import com.nice.service.StockDetailsService;
import com.nice.service.StockTransferService;
import com.nice.service.TaskService;
import com.nice.service.TicketReasonService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.service.WalletTrxService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Service(value = "orderService")
@Transactional(rollbackFor = Throwable.class)
public class OrdersServiceImpl implements OrdersService {

	/**
	 *
	 */
	private static final String NOT_FOUND = "order.not.found";

	private static final Logger LOGGER = LoggerFactory.getLogger(OrdersServiceImpl.class);

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
	private StateService stateService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private VendorService vendorService;

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
	private OrderToppingsRepository orderToppingsRepository;

	@Autowired
	private OrderExtrasRepository orderExtrasRepository;

	@Autowired
	private OrderAddonsRepository orderAddonsRepository;

	@Autowired
	private OrderProductAttributeValueRepository orderProductAttributeValueRepository;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private DeliveryBoySendNotificationHistoryRepository deliveryBoySendNotificationHistoryRepository;

	@Autowired
	private StockDetailsService stockDetailsService;

	@Autowired
	private StockAllocationService stockAllocationService;

	@Autowired
	private StockTransferService stockTransferService;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private BusinessCategoryService businessCategoryService;

	@Autowired
	private HesabePaymentService hesabePaymentService;

	@Autowired
	private OrderRatingRepository orderRatingRepository;

	@Autowired
	private OrderRatingMapper orderRatingMapper;

	@Autowired
	private WalletTrxService walletTrxService;

	@Autowired
	private TicketReasonService ticketReasonService;

	@Value("${service.url}")
	private String serviceUrl;

	@Autowired
	private RatingQuestionService ratingQuestionService;

	@Autowired
	private RatingQuestionMapper ratingQuestionMapper;

	@Autowired
	private TaskService taskService;

	@Override
	public String validateOrder(final OrderRequestDTO orderRequestDto) throws ValidationException, NotFoundException {

		if (!orderRequestDto.getDeliveryType().equals(DeliveryType.PICKUP.getStatusValue())
				&& !orderRequestDto.getDeliveryType().equals(DeliveryType.DELIVERY.getStatusValue())) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.delivery.type", null));
		}
		if (!orderRequestDto.getPaymentMode().equals(PaymentMode.COD.name()) && !orderRequestDto.getPaymentMode().equals(PaymentMode.ONLINE.name())) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.mode", null));
		}

		/**
		 * COD is not supported for pickup orders
		 */
		if (DeliveryType.PICKUP.getStatusValue().equalsIgnoreCase(orderRequestDto.getDeliveryType())
				&& PaymentMode.COD.name().equalsIgnoreCase(orderRequestDto.getPaymentMode())) {
			throw new ValidationException(messageByLocaleService.getMessage("cod.not.supported.pickup", null));
		}

		Long customerId = getCustomerIdForLoginUser();
		orderRequestDto.setCustomerId(customerId);

		/**
		 * Check if the customer has any ongoing orders
		 */
		Long count = ongoingOrderCount(customerId);
		if (count > 0) {
			throw new ValidationException(messageByLocaleService.getMessage("ongoing.order.present", null));
		}

		CustomerAddress customerAddress = customerAddressService.getAddressDetails(orderRequestDto.getShippingAddressId());
		City city = customerAddress.getCity();
		Customer customer = customerService.getCustomerDetails(customerId);
		List<CartItem> cartItemList = cartItemService.getCartListBasedOnCustomer(orderRequestDto.getCustomerId());
		if (cartItemList.isEmpty()) {
			throw new ValidationException(messageByLocaleService.getMessage("order.unavailable", null));
		}
		Long vendorId = cartItemList.get(0).getProductVariant().getVendorId();
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		orderRequestDto.setApplyDeliveryCharge(true);
		if (DeliveryType.PICKUP.getStatusValue().equals(orderRequestDto.getDeliveryType())) {
			orderRequestDto.setApplyDeliveryCharge(false);
		}
		/**
		 * Check if the vendor servicable and customer delivery belong to same city
		 */
		if (!vendor.getActive() || !VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus()) || !vendor.getIsOrderServiceEnable()) {
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
		 * check if the products in cart are active or not active then throw error also
		 * check for the available quantity.
		 */
		for (CartItem cartItem : cartItemList) {
			ProductVariant productVariant = productVariantService.getProductVariantDetail(cartItem.getProductVariant().getId());
			if (!productVariant.getActive().booleanValue()) {
				if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
					throw new ValidationException(messageByLocaleService.getMessage("product.inactive",
							new Object[] { productVariant.getProduct().getNameEnglish(), productVariant.getUom().getMeasurementEnglish() }));
				} else {
					throw new ValidationException(messageByLocaleService.getMessage("product.inactive",
							new Object[] { productVariant.getProduct().getNameArabic(), productVariant.getUom().getMeasurementArabic() }));
				}

			} else {
				/**
				 * Stock related check for product while placing order by customer for Grocery
				 * business category in which the inventory
				 * is managed
				 */
				BusinessCategory businessCategory = vendor.getBusinessCategory();
				if (businessCategory.getManageInventory()) {
					Long availableQty = stockDetailsService.getCountForVariant(productVariant);
					if (availableQty == null || availableQty < cartItem.getQuantity()) {
						if ("en".equalsIgnoreCase(LocaleContextHolder.getLocale().getLanguage())) {
							throw new ValidationException(messageByLocaleService.getMessage("insufficient.stock.for.product.available.qty", new Object[] {
									productVariant.getProduct().getNameEnglish(), productVariant.getUom().getMeasurementEnglish(), availableQty }));
						} else {
							throw new ValidationException(messageByLocaleService.getMessage("insufficient.stock.for.product.available.qty", new Object[] {
									productVariant.getProduct().getNameArabic(), productVariant.getUom().getMeasurementArabic(), availableQty }));
						}

					}
				}

			}
		}

		/**
		 * This amount includes amount with delivery charge, the wallet contribution
		 * would be calculated after that and
		 * subtracted from the actual order amount.
		 */
		Double calculatedOrderAmtWithOutDelCharge = calculateTotalOrderAmt(cartItemList);

		/**
		 * Check if the order amount is greater than the minimum order amount of vendor,
		 * else throw an exception
		 */
		if (calculatedOrderAmtWithOutDelCharge.compareTo(vendor.getMinimumOrderAmt()) < 0) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.minimum.order.amount", new Object[] { vendor.getMinimumOrderAmt() }));
		}
		Double calculatedOrderAmt = addDeliveryCharge(orderRequestDto.getApplyDeliveryCharge(), calculatedOrderAmtWithOutDelCharge);
		Double amountAfterWalletDeduction = calculatedOrderAmt;
		/**
		 * Check for wallet amount if the wallet amount is to be utilized
		 */
		if (orderRequestDto.getUseWallet()) {
			if (customer.getWalletAmt() > amountAfterWalletDeduction) {
				orderRequestDto.setWalletContribution(amountAfterWalletDeduction);
				amountAfterWalletDeduction = 0.0d;
			} else {
				amountAfterWalletDeduction = Double.sum(amountAfterWalletDeduction, customer.getWalletAmt() * -1);
				orderRequestDto.setWalletContribution(customer.getWalletAmt());
			}

		} else {
			orderRequestDto.setWalletContribution(0.0d);
		}

		/**
		 * Validate order amount
		 */
		if (!amountAfterWalletDeduction.equals(CommonUtility.round(orderRequestDto.getTotalOrderAmount()))) {
			throw new ValidationException(messageByLocaleService.getMessage("order.amount.mismatch", new Object[] { amountAfterWalletDeduction }));
		}

		/**
		 * Validate and prepare order object
		 */
		if (orderRequestDto.getPaymentMode().equalsIgnoreCase(PaymentMode.COD.name()) || amountAfterWalletDeduction == 0.0d) {
			Orders order = createOrder(cartItemList, orderRequestDto, amountAfterWalletDeduction);

			/**
			 * remove items from cart
			 */
			cartItemService.deleteCartItemForCustomer(orderRequestDto.getCustomerId());

			return String.valueOf(order.getId());
		} else {
			if (calculatedOrderAmt.equals(0D)) {
				throw new ValidationException(messageByLocaleService.getMessage("order.amt.non.zero", null));
			}

			String onlineOrderId = System.currentTimeMillis() + "_order_" + customerId;
			for (CartItem cartItem : cartItemList) {
				OnlineCart onlineCart = new OnlineCart();
				onlineCart.setCustomer(cartItem.getCustomer());
				onlineCart.setProductVariant(cartItem.getProductVariant());
				onlineCart.setQuantity(cartItem.getQuantity());

				onlineCart.setCityId(customerAddress.getCity().getId());
				onlineCart.setStateId(customerAddress.getState().getId());
				onlineCart.setPincodeId(customerAddress.getPincode().getId());
				onlineCart.setAddressEnglish(makeCustomerAddressEnglish(customerAddress));
				onlineCart.setAddressArabic(makeCustomerAddressArabic(customerAddress));
				onlineCart.setLatitude(customerAddress.getLatitude());
				onlineCart.setLongitude(customerAddress.getLongitude());
				onlineCart.setFirstName(customerAddress.getFirstName());
				onlineCart.setLastName(customerAddress.getLastName());
				onlineCart.setPhoneNumber(customerAddress.getPhoneNumber());
				onlineCart.setDeliveryType(orderRequestDto.getDeliveryType());
				onlineCart.setDescription(orderRequestDto.getDescription());
				onlineCart.setStatus(CartItemStatus.PAYMENT_WAITING.getStatusValue());
				onlineCart.setPaymentAmount(amountAfterWalletDeduction);
				cartItem.setOnlineOrderId(onlineOrderId);
				onlineCart.setOnlineOrderId(onlineOrderId);
				onlineCart.setActive(true);
				/**
				 * Set wallet contribution amount
				 */
				onlineCart.setWalletContirbution(orderRequestDto.getWalletContribution());
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
			String redirectUrl = serviceUrl.concat("payment/check");
			LOGGER.info("inside hesabe gateway for generate url");
			String url = hesabePaymentService.createPaymentGateway(onlineOrderId, amountAfterWalletDeduction, redirectUrl);
			LOGGER.info("outside hesabe gateway for generate url");
			return url;
		}
	}

	/**
	 * This method is used to check if the customer has any ongoing orders
	 *
	 * @param  customerId
	 * @return
	 */
	private Long ongoingOrderCount(final Long customerId) {
		return ordersRepository.getCountofOngoingOrdersForCustomer(customerId, Constant.getCompletedOrderStatusList());
	}

	private String makeCustomerAddressEnglish(final CustomerAddress customerAddress) {
		return customerAddress.getStreetNo().concat(" ").concat(customerAddress.getBuildingName()).concat(" ").concat(customerAddress.getArea()).concat(" ")
				.concat(customerAddress.getCity().getNameEnglish()).concat(" ").concat(customerAddress.getPincode().getCodeValue()).concat(" ")
				.concat(customerAddress.getState().getNameEnglish());
	}

	private String makeCustomerAddressArabic(final CustomerAddress customerAddress) {
		return customerAddress.getStreetNo().concat(" ").concat(customerAddress.getBuildingName()).concat(" ").concat(customerAddress.getArea()).concat(" ")
				.concat(customerAddress.getCity().getNameArabic()).concat(" ").concat(customerAddress.getPincode().getCodeValue()).concat(" ")
				.concat(customerAddress.getState().getNameArabic());
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
	 * @param  cartItemList
	 * @param  orderRequestDto
	 * @param  calculatedOrderAmt
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

		/**
		 * Check for wallet amount
		 */
		if (orderRequestDto.getWalletContribution() != 0 && orderRequestDto.getWalletContribution().compareTo(customer.getWalletAmt()) > 0) {
			throw new ValidationException(messageByLocaleService.getMessage("wallet.amount.insufficient", null));
		}
		order.setActive(true);
		order.setPaymentMode(orderRequestDto.getPaymentMode());
		order.setDeliveryType(orderRequestDto.getDeliveryType());
		order.setAssignmentTryCount(0);
		order.setNotificationTimer(new Date(System.currentTimeMillis()));
		order.setDescription(orderRequestDto.getDescription());
		order.setRefunded(false);
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
			order.setAddressEnglish(makeCustomerAddressEnglish(customerAddress));
			order.setAddressArabic(makeCustomerAddressArabic(customerAddress));
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
		 * else we will get the address details from razor pay cart with values set in
		 * orderRequestDto
		 */
		else {
			Pincode pincode = pincodeService.getPincodeDetails(orderRequestDto.getPincodeId());
			State state = stateService.getStateDetails(orderRequestDto.getStateId());
			City city = cityService.getCityDetails(orderRequestDto.getCityId());

			order.setAddressEnglish(orderRequestDto.getAddressEnglish());
			order.setAddressArabic(orderRequestDto.getAddressArabic());
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

		Double distance = 0.0d;
		if (!DeliveryType.PICKUP.getStatusValue().equals(orderRequestDto.getDeliveryType())) {
			BigDecimal pickUpLatitude = vendor.getLatitude();
			BigDecimal pickupLongitude = vendor.getLongitude();
			BigDecimal dropLatitude = order.getLatitude();
			BigDecimal dropLongitude = order.getLongitude();

			distance = CommonUtility.distance(pickUpLatitude.doubleValue(), pickupLongitude.doubleValue(), dropLatitude.doubleValue(),
					dropLongitude.doubleValue());
		}

		order.setDistance(distance);
		order.setOrderStatus(OrderStatusEnum.PENDING.getStatusValue());
		order.setTotalOrderAmount(calculatedOrderAmt);
		order.setWalletContribution(orderRequestDto.getWalletContribution());
		if (calculatedOrderAmt == 0.0d) {
			order.setPaymentMode(PaymentMode.WALLET.name());
		}

		/**
		 * Set Online Payment details for Order
		 */
		if (orderRequestDto.getOnlineOrderId() != null && !orderRequestDto.getOnlineOrderId().isBlank()) {
			order.setOnlineOrderId(orderRequestDto.getOnlineOrderId());
			order.setOnlinePaymentToken(orderRequestDto.getPaymentToken());
			order.setPaymentId(orderRequestDto.getPaymentId());
			order.setAdministrativeCharge(orderRequestDto.getAdministrativeCharge());
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
				orderItem.setTotalDiscountAmt(
						orderItem.getUnitPrice() * cartItem.getQuantity() - orderItem.getUnitPriceAfterDiscount() * cartItem.getQuantity());
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
				orderItemTotal += orderAddons.getAmount();
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
				orderItemTotal += orderExtras.getAmount();
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
				orderItemTotal += orderProductAttributeValue.getAmount();
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
				orderItemTotal += orderToppings.getAmount();
				orderToppingsList.add(orderToppings);
			}
			orderItem.setOrderToppingsList(orderToppingsList);
		}

		/**
		 * Check if delivery charge is applicable
		 */
		Double deliveryCharge = (Double) SettingsConstant.getSettingsValue(Constant.ORDER_DELIVERY_CHARGE);
		Double orderAmountForFreeDelivery = (Double) SettingsConstant.getSettingsValue(Constant.ORDER_AMOUNT_FOR_FREE_DELIVERY);
		/**
		 * If there is any configuration related to minimum order amount, this is the
		 * configuration for the same. If delivery
		 * charge is to be taken for all order set the value to any negative value
		 */
		if (!DeliveryType.PICKUP.getStatusValue().equals(orderRequestDto.getDeliveryType())
				&& (orderAmountForFreeDelivery < 0 || orderItemTotal < orderAmountForFreeDelivery)) {
			order.setDeliveryCharge(deliveryCharge);
		} else {
			order.setDeliveryCharge(0d);
		}
		order.setGrossOrderAmount(orderItemTotal);
		order.setReplaced(false);
		if (PaymentMode.ONLINE.name().equals(order.getPaymentMode())) {
			order.setPaymentDate(new Date());
		}
		ordersRepository.save(order);

		/**
		 * Make an entry in wallet txn table
		 */
		if (orderRequestDto.getWalletContribution() != 0) {
			addWalletTxn(orderRequestDto.getWalletContribution() * -1, orderRequestDto.getCustomerId(), order.getId(), null);
			/**
			 * Set updated wallet balance
			 */
			Double updatedWalletBalance = customer.getWalletAmt() - orderRequestDto.getWalletContribution();
			customerService.updateWalletBalance(updatedWalletBalance, customer.getId());
		}

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
	 * @param  description
	 * @param  orderRequestDto
	 * @param  order
	 * @throws NotFoundException
	 */
	private void addWalletTxn(final Double transactionAmount, final Long customerId, final Long orderId, final String description) throws NotFoundException {
		WalletTrxDTO walletTxnDto = new WalletTrxDTO();
		walletTxnDto.setActive(true);
		walletTxnDto.setAmount(transactionAmount);
		walletTxnDto.setCustomerId(customerId);
		walletTxnDto.setOrderId(orderId);
		walletTxnDto.setDescription(description);
		walletTrxService.addupdateWalletTrx(walletTxnDto);
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

	@Override
	public Double calculateTotalOrderAmt(final List<CartItem> cartItemList) throws NotFoundException {

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
			orderAmt = orderAmt + rate * cartItem.getQuantity() + cartProductAttributeListAmount + totalToppingsAmount + totalExtrasAmount + totalAddonsAmount;
		}
		return CommonUtility.round(orderAmt);
	}

	/**
	 * @param  applyDeliveryCharge
	 * @param  orderAmt
	 * @return
	 */
	@Override
	public Double addDeliveryCharge(final boolean applyDeliveryCharge, Double orderAmt) {
		/**
		 * Check if delivery charge is applicable
		 */
		Double deliveryCharge = (Double) SettingsConstant.getSettingsValue(Constant.ORDER_DELIVERY_CHARGE);
		Double orderAmountForFreeDelivery = (Double) SettingsConstant.getSettingsValue(Constant.ORDER_AMOUNT_FOR_FREE_DELIVERY);
		/**
		 * If there is any configuration related to minimum order amount, this is the
		 * configuration for the same. If delivery
		 * charge is to be taken for all order set the value to any negative value
		 */
		if (applyDeliveryCharge && (orderAmountForFreeDelivery < 0 || orderAmt < orderAmountForFreeDelivery)) {
			orderAmt = Double.sum(orderAmt, deliveryCharge);
		}
		return orderAmt;
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
			throws NotFoundException, ValidationException {
		final Locale locale = LocaleContextHolder.getLocale();
		OrdersResponseDTO orderResponseDto = new OrdersResponseDTO();
		BeanUtils.copyProperties(orders, orderResponseDto);
		/**
		 * If status is stock allocted then for display purpose we will display as
		 * waiting for pickup
		 */
		if (OrderStatusEnum.STOCK_ALLOCATED.getStatusValue().equals(orders.getOrderStatus())) {
			orderResponseDto.setOrderStatus(Constant.WAITING_FOR_PICKUP);
		}

		/**
		 * set city field for email
		 */
		if (locale.getLanguage().equals("en")) {
			Vendor vendor = orders.getVendor();
			orderResponseDto.setCity(orders.getCity().getNameEnglish());
			orderResponseDto.setVendorName(vendor.getStoreNameEnglish());
			orderResponseDto.setAddress(orders.getAddressEnglish());
			orderResponseDto.setReplace(Constant.RETURN.equalsIgnoreCase(vendor.getAccepts()));
		} else {
			Vendor vendor = orders.getVendor();
			orderResponseDto.setCity(orders.getCity().getNameArabic());
			orderResponseDto.setVendorName(vendor.getStoreNameArabic());
			orderResponseDto.setAddress(orders.getAddressArabic());
			orderResponseDto.setReplace(Constant.RETURN.equalsIgnoreCase(vendor.getAccepts()));
		}
		/**
		 * Set reason for the order if any
		 */
		if (orders.getReturnReplaceReason() != null) {
			TicketReason reason = orders.getReturnReplaceReason();
			if (locale.getLanguage().equals("en")) {
				if (TicketReasonType.CANCEL.name().equals(reason.getType())) {
					orderResponseDto.setCancelReason(reason.getReasonEnglish());
				} else if (TicketReasonType.REJECT.name().equals(reason.getType())) {
					orderResponseDto.setRejectReason(reason.getReasonEnglish());
				} else if (TicketReasonType.REPLACE.name().equals(reason.getType())) {
					orderResponseDto.setReplaceReason(reason.getReasonEnglish());
				} else if (TicketReasonType.RETURN.name().equals(reason.getType())) {
					orderResponseDto.setReturnReason(reason.getReasonEnglish());
				}
			} else {
				if (TicketReasonType.CANCEL.name().equals(reason.getType())) {
					orderResponseDto.setCancelReason(reason.getReasonArabic());
				} else if (TicketReasonType.REJECT.name().equals(reason.getType())) {
					orderResponseDto.setRejectReason(reason.getReasonArabic());
				} else if (TicketReasonType.REPLACE.name().equals(reason.getType())) {
					orderResponseDto.setReplaceReason(reason.getReasonArabic());
				} else if (TicketReasonType.RETURN.name().equals(reason.getType())) {
					orderResponseDto.setReturnReason(reason.getReasonArabic());
				}
			}

		}

		/**
		 * set pincode field for email and push notification
		 */
		orderResponseDto.setPincode(orders.getPincode().getCodeValue());

		if (!isFromAdmin) {
			orderResponseDto.setCustomerName(orders.getFirstName().concat(" ").concat(orders.getLastName()));
			orderResponseDto.setPhoneNumber(orders.getPhoneNumber());
		} else {

			if (orders.getPaymentMode().equals(PaymentMode.ONLINE.name())) {
				orderResponseDto.setPaymentId(orders.getPaymentId());
				orderResponseDto.setPaymentToken(orders.getOnlinePaymentToken());
				orderResponseDto.setAdministrativeCharge(orders.getAdministrativeCharge());
				orderResponseDto.setHesabeOrderId(orders.getOnlineOrderId());
			}

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
			orderResponseDto.setCustomerId(customer.getId());
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
			orderResponseDto.setDeliveryBoyId(orders.getDeliveryBoy().getId());
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
			orderResponseDto.setReplacementDeliveryBoyId(orders.getReplacementDeliveryBoy().getId());
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
		BusinessCategoryDTO businessCategory = businessCategoryService.getBusinessCategory(vendorDto.getBusinessCategoryId());
		orderResponseDto.setManageInventory(businessCategory.getManageInventory());
		orderResponseDto.setVendorPhoneNumber(vendorDto.getStorePhoneNumber());
		return orderResponseDto;
	}

	/**
	 * @param  orders
	 * @param  orderResponseDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private Long setOrderItemInResponse(final Orders orders, final OrdersResponseDTO orderResponseDto) throws NotFoundException, ValidationException {
		List<OrdersItem> orderItemList = ordersItemRepository.findAllByOrderId(orders.getId());
		orderResponseDto.setOrderItemResponseDtoList(orderItemService.toOrderItemResponseDto(orderItemList));
		Long totalCountForOrder = 0L;
		for (OrdersItem orderItem : orderItemList) {
			totalCountForOrder += orderItem.getQuantity();
		}
		return totalCountForOrder;
	}

	private List<OrdersResponseDTO> toDtos(final List<Orders> orders, final boolean isFromAdmin) throws NotFoundException, ValidationException {
		List<OrdersResponseDTO> results = new ArrayList<>();
		for (Orders o : orders) {
			results.add(toDto(o, isFromAdmin, false, false));
		}
		return results;
	}

	@Override
	public void changeStatus(String newStatus, final Orders order) throws NotFoundException, ValidationException {
		if (order == null || order.getId() == 0) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.order.change.status", null));
		}
		UserLogin userLogin = checkForUserLogin();

		/**
		 * Validation for allowing vendor only to mark status as "Order Pick Up" and
		 * that too only for PickUp Order, else
		 * placing a validation allowing only delivery boy to do the same
		 */
		if (newStatus.equals(OrderStatusEnum.ORDER_PICKED_UP.getStatusValue()) && (DeliveryType.PICKUP.getStatusValue().equals(order.getDeliveryType())
				&& !UserType.VENDOR.name().equals(userLogin.getEntityType())
				|| DeliveryType.DELIVERY.getStatusValue().equals(order.getDeliveryType()) && !UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType()))) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		String allocatedFor = TaskTypeEnum.DELIVERY.name();
		OrderStatusEnum existingOrderStatus = OrderStatusEnum.getByValue(order.getOrderStatus());
		final String existingStockStatus = existingOrderStatus.getStockValue();
		if (!existingOrderStatus.contains(newStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("status.not.allowed", new Object[] { newStatus, order.getOrderStatus() }));
		}
		/**
		 * Check manage inventory flag for order, if its true then need to place a check
		 * that once the order is in "Order Is
		 * Ready" status it is not directly moved to Order Pickup before allocating
		 * stock
		 */
		OrdersResponseDTO ordersResponseDto = getOrderDetails(order.getId());
		if (ordersResponseDto.getManageInventory().booleanValue()
				&& OrderStatusEnum.ORDER_IS_PREPARED.getStatusValue().equals(existingOrderStatus.getStatusValue())
				&& !OrderStatusEnum.STOCK_ALLOCATED.getStatusValue().equals(newStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("allocate.stock.first", null));
		}

		/**
		 * Check if the vendor confirms the order and its delivery type is Pick Up, then
		 * make the status as in Process for the
		 * order. </br>
		 * if the order is pickuped in case of Pickup orders, it will be marked as
		 * delivered
		 */
		if (DeliveryType.PICKUP.getStatusValue().equals(order.getDeliveryType())) {
			String taskType = null;
			if (OrderStatusEnum.CONFIRMED.getStatusValue().equals(newStatus)) {
				order.setOrderStatus(newStatus);
				ordersRepository.save(order);
				saveOrderStatusHistory(order);
				taskType = TaskTypeEnum.DELIVERY.getTaskValue();
				newStatus = OrderStatusEnum.IN_PROCESS.getStatusValue();
			} else if (OrderStatusEnum.RETURN_CONFIRMED.getStatusValue().equals(newStatus)) {
				order.setOrderStatus(newStatus);
				ordersRepository.save(order);
				saveOrderStatusHistory(order);
				taskType = TaskTypeEnum.RETURN.getTaskValue();
				newStatus = OrderStatusEnum.RETURN_PROCESSED.getStatusValue();
			} else if (OrderStatusEnum.REPLACE_CONFIRMED.getStatusValue().equals(newStatus)) {
				order.setOrderStatus(newStatus);
				ordersRepository.save(order);
				saveOrderStatusHistory(order);
				taskType = TaskTypeEnum.REPLACEMENT.getTaskValue();
				newStatus = OrderStatusEnum.REPLACE_PROCESSED.getStatusValue();
			} else {
				LOGGER.info("Task type is not confirmed");
			}
			if (taskType != null) {
				/**
				 * Create task for the order
				 */
				TaskDto taskDto = new TaskDto();
				taskDto.setOrderId(order.getId());
				taskDto.setTaskType(taskType);
				taskService.createTask(taskDto);
			}
		}

		/**
		 * check for manage inventory flag, if that is true then stock needs to be
		 * allocated for the order, else move the order
		 * from Order_Prepared Status to Stock_Allocated status
		 */
		if (!ordersResponseDto.getManageInventory().booleanValue() && OrderStatusEnum.ORDER_IS_PREPARED.getStatusValue().equals(newStatus)) {
			/**
			 * If order is ready move it to the stock allocation status directly for orders
			 * not requiring any stock allocation for
			 * order.
			 */
			order.setOrderStatus(newStatus);
			ordersRepository.save(order);
			saveOrderStatusHistory(order);

			/**
			 * Set new status to stock allocation
			 */
			newStatus = OrderStatusEnum.STOCK_ALLOCATED.getStatusValue();
		}

		if (OrderStatusEnum.DELIVERED.getStatusValue().equals(newStatus)) {
			order.setDeliveryDate(new Date());
			if (PaymentMode.COD.name().equals(order.getPaymentMode())) {
				order.setPaymentDate(new Date());
			}
		}

		if (newStatus.equalsIgnoreCase(Constant.RETURN_REQUESTED) || newStatus.equalsIgnoreCase(Constant.RETURN_CONFIRMED)
				|| newStatus.equalsIgnoreCase(Constant.RETURN_REJECTED) || newStatus.equalsIgnoreCase(Constant.RETURN_PROCESSED)
				|| newStatus.equalsIgnoreCase(Constant.RETURN_ORDER_PICKUP) || newStatus.equalsIgnoreCase(Constant.RETURNED)) {
			allocatedFor = TaskTypeEnum.RETURN.name();
		}

		order.setOrderStatus(newStatus);
		ordersRepository.save(order);

		saveOrderStatusHistory(order);

		/**
		 * If the order status is related to replacement then set allocatedFor as
		 * Replacement
		 */
		if (newStatus.equalsIgnoreCase(Constant.REPLACE_REQUESTED) || newStatus.equalsIgnoreCase(Constant.REPLACE_PROCESSED)
				|| newStatus.equalsIgnoreCase(Constant.REPLACED)) {
			allocatedFor = TaskTypeEnum.REPLACEMENT.name();
		}

		/**
		 * Change inventory based on status
		 */
		/**
		 * Here if the existing stock status is delivered then we dont need to transfer
		 * the inventory, that will be a typical
		 * case of replacement of orders that will be handled in a different way
		 */
		if (!Constant.DELIVERED.equalsIgnoreCase(existingStockStatus)
				&& !existingStockStatus.equalsIgnoreCase(OrderStatusEnum.getByValue(order.getOrderStatus()).getStockValue())) {
			/**
			 * Fetch list of all allocated stock based on lot and move one by one for the
			 * order.
			 */
			List<StockAllocation> stockAllocationList = stockAllocationService.getAllocatedStockForOrder(order.getId(), allocatedFor);
			for (StockAllocation stockAllocation : stockAllocationList) {
				StockTransferDto stockTransferDto = new StockTransferDto();
				stockTransferDto.setTransferedFrom(existingStockStatus);
				stockTransferDto.setTransferedTo(OrderStatusEnum.getByValue(order.getOrderStatus()).getStockValue());
				stockTransferDto.setStockDetailsId(stockAllocation.getStockDetails().getId());
				stockTransferDto.setQuantity(stockAllocation.getQuantity());
				stockTransferDto.setOrderId(order.getId());
				stockTransferDto.setVendorId(stockAllocation.getVendorId());
				ProductVariantResponseDTO productVariant = productVariantService
						.getProductVariant(stockAllocation.getStockDetails().getProductVariant().getId());
				stockTransferDto.setProductId(productVariant.getProductId());
				stockTransferDto.setUomId(productVariant.getUomId());
				stockTransferDto.setLotNo(stockAllocation.getStockDetails().getLotNo());
				stockTransferService.transferStock(stockTransferDto);
			}
		}
		/**
		 * This handles the Replacement of stock, the stock already delivered for a
		 * order will be moved from delivered to
		 * replaced status
		 */
		if (newStatus.equalsIgnoreCase(Constant.REPLACED)) {
			List<StockAllocation> stockAllocationList = stockAllocationService.getAllocatedStockForOrder(order.getId(), TaskTypeEnum.REPLACEMENT.name());
			Set<Long> orderItemIdSet = new HashSet<>();
			for (StockAllocation stockAllocation : stockAllocationList) {
				orderItemIdSet.add(stockAllocation.getOrderItem().getId());
			}
			for (Long orderItem : orderItemIdSet) {
				List<StockAllocation> replacementStockAllocationList = stockAllocationService.getAllocatedStockForOrderItem(orderItem,
						TaskTypeEnum.DELIVERY.name());
				for (StockAllocation stockAllocation : replacementStockAllocationList) {
					StockTransferDto stockTransferDto = new StockTransferDto();
					stockTransferDto.setTransferedFrom(Constant.DELIVERED);
					stockTransferDto.setTransferedTo(Constant.REPLACED);
					stockTransferDto.setStockDetailsId(stockAllocation.getStockDetails().getId());
					stockTransferDto.setQuantity(stockAllocation.getQuantity());
					stockTransferDto.setOrderId(order.getId());
					stockTransferDto.setVendorId(stockAllocation.getVendorId());

					ProductVariantResponseDTO productVariant = productVariantService
							.getProductVariant(stockAllocation.getStockDetails().getProductVariant().getId());
					stockTransferDto.setProductId(productVariant.getProductId());
					stockTransferDto.setUomId(productVariant.getUomId());
					stockTransferDto.setLotNo(stockAllocation.getStockDetails().getLotNo());
					stockTransferService.transferStock(stockTransferDto);
				}
			}
		}
	}

	@Override
	public Orders getOrder(final Long orderId) throws NotFoundException {
		return ordersRepository.findById(orderId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { orderId })));
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

		Orders order = getOrder(orderId);
		/**
		 * If the user is Vendor or customer, check if the order actually belongs to
		 * him.
		 */
		if (!isFromAdmin && !order.getCustomer().getId().equals(customerId) || isFromAdmin && vendorId != null && !order.getVendor().getId().equals(vendorId)) {
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
		if (OrderStatusEnum.CANCELLED.getStatusValue().equals(ordersResponseDTO.getOrderStatus())) {
			for (OrderStatusDto orderStatusDto : orderStatusDtoList) {
				if (OrderStatusEnum.CANCELLED.getStatusValue().equals(orderStatusDto.getStatus())) {
					ordersResponseDTO.setCancelDate(orderStatusDto.getCreatedAt());
					UserLogin userLoginTemp = userLoginService.getUserLoginDetail(orderStatusDto.getCreatedBy());
					if (userLoginTemp.getEntityType().equalsIgnoreCase(UserType.CUSTOMER.name())) {
						CustomerResponseDTO customerResponseDto = customerService.getCustomer(userLoginTemp.getEntityId());
						ordersResponseDTO.setCancelledBy(customerResponseDto.getName());
					} else {
						ordersResponseDTO.setCancelledBy(messageByLocaleService.getMessage("nice", null));
					}
				}
			}
		}

		List<RatingQuestion> ratingQuestion = new ArrayList<>();
		if (ordersResponseDTO.getDeliveryType().equalsIgnoreCase(DeliveryType.PICKUP.name())) {
			ratingQuestion = ratingQuestionService.getList(1, 1000, "Vendor").getContent();
		} else {
			ratingQuestion = ratingQuestionService.getList(1, 1000, null).getContent();
		}
		ordersResponseDTO.setRatingQuestionList(ratingQuestionMapper.toDtos(ratingQuestion));
		Optional<OrderRating> orderRating = orderRatingRepository.findByOrderId(orderId);
		if (orderRating.isPresent()) {
			ordersResponseDTO.setOrderRating(orderRatingMapper.toResponseDto(orderRating.get()));
		}
		return ordersResponseDTO;
	}

	@Override
	public Optional<Orders> getOrderDetailsByOnlineOrderId(final String onlineOrderId) {
		return ordersRepository.findByOnlineOrderId(onlineOrderId);
	}

	@Override
	public boolean validateUser(final Long userId, final Long entityId, final List<String> userType)
			throws NotFoundException, AuthorizationException, ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (Role.SUPER_ADMIN.getStatusValue().equals(userLogin.getRole().getName())) {
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
			throws NotFoundException, FileNotFoundException, ValidationException {
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
	public void cancelOrder(final ReplaceCancelOrderDto replaceCancelOrderDto, final boolean autoRefund) throws NotFoundException, ValidationException {
		Orders orders = getOrderById(replaceCancelOrderDto.getOrderId());
		if (OrderStatusEnum.DELIVERED.getStatusValue().equals(orders.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("cannot.cancel.deliverd.order", null));
		}
		/**
		 * Cancel task if exists for the order
		 */
		List<Task> taskList = taskService.getTaskListForOrderId(orders.getId());
		if (taskList.size() > 1) {
			throw new ValidationException(messageByLocaleService.getMessage("orders.return.replace.not.cancelled", null));
		} else if (!taskList.isEmpty()) {
			taskService.changeTaskStatus(taskList.get(0).getId(), TaskStatusEnum.CANCELLED.getStatusValue());
		}

		TicketReason ticketReason = ticketReasonService.getTicketReasonDetails(replaceCancelOrderDto.getReasonId());

		orders.setReturnReplaceReason(ticketReason);
		orders.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
		/**
		 * Add this method to cancel order.
		 */
		orders.setOrderStatus(OrderStatusEnum.CANCELLED.getStatusValue());
		if (autoRefund) {
			orders.setRefunded(true);
		}
		ordersRepository.save(orders);

		saveOrderStatusHistory(orders);
		/**
		 * Update the wallet for the customer, incase of online payment
		 */
		if (!PaymentMode.COD.name().equals(orders.getPaymentMode()) && autoRefund) {
			/**
			 * this means the refund is to be made to customer wallet after deducting the
			 * delivery charges from the order
			 */
			Double amountToBeCredited = orders.getTotalOrderAmount() + orders.getWalletContribution() - orders.getDeliveryCharge();
			customerService.updateWalletBalance(amountToBeCredited, orders.getCustomer().getId());
			/**
			 * make an entry in wallet txn
			 */
			addWalletTxn(amountToBeCredited, orders.getCustomer().getId(), orders.getId(), null);
		}

		/**
		 * Cancel task if exists for the order
		 */
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
			 * If the replacement request has come after a maximum days for which vendor can
			 * accepts then throw error.
			 */
			if (CommonUtility.convertDateToLocalDateTime(orderStatusHistory.get().getCreatedAt()).plusDays(orders.getVendor().getMaxDaysForAccept())
					.isBefore(LocalDateTime.now())) {
				throw new ValidationException(
						messageByLocaleService.getMessage("order.outside.replacement.period", new Object[] { orders.getVendor().getMaxDaysForAccept() }));
			}
			TicketReason ticketReason = ticketReasonService.getTicketReasonDetails(replaceCancelOrderDto.getReasonId());
			orders.setReturnReplaceReason(ticketReason);
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
			 * If the return request has come after a maximum days for which vendor can
			 * accepts then throw error.
			 */
			if (CommonUtility.convertDateToLocalDateTime(orderStatusHistory.get().getCreatedAt()).plusDays(orders.getVendor().getMaxDaysForAccept())
					.isBefore(LocalDateTime.now())) {
				throw new ValidationException(
						messageByLocaleService.getMessage("order.outside.return.period", new Object[] { orders.getVendor().getMaxDaysForAccept() }));
			}
			TicketReason ticketReason = ticketReasonService.getTicketReasonDetails(replaceCancelOrderDto.getReasonId());
			orders.setReturnReplaceReason(ticketReason);
			orders.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
			changeStatus(Constant.RETURN_REQUESTED, orders);
		}
	}

	@Override
	public void changeStatus(final Long ordersId, final String status) throws NotFoundException, ValidationException {
		Orders orders = getOrderById(ordersId);
		changeStatus(status, orders);
	}

	@Override
	public void rejectOrder(final ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException {

		Orders orders = getOrderById(replaceCancelOrderDto.getOrderId());
		OrderStatusEnum existingOrderStatus = OrderStatusEnum.getByValue(orders.getOrderStatus());
		if ((!existingOrderStatus.contains(Constant.REJECTED) && !existingOrderStatus.contains(Constant.RETURN_REJECTED) && !existingOrderStatus.contains(Constant.REPLACE_REJECTED))) {
			String newOrderStatusForMessage = messageByLocaleService.getMessage(Constant.REJECTED, null);
			String existingOrderStatusForMessage = messageByLocaleService.getMessage(orders.getOrderStatus(), null);
			throw new ValidationException(
					messageByLocaleService.getMessage("status.not.allowed", new Object[] { newOrderStatusForMessage, existingOrderStatusForMessage }));
		}
		if (OrderStatusEnum.RETURN_REQUESTED.getStatusValue().equals(orders.getOrderStatus())) {
			orders.setOrderStatus(Constant.RETURN_REJECTED);
		} else if (OrderStatusEnum.REPLACE_REQUESTED.getStatusValue().equals(orders.getOrderStatus())) {
			orders.setOrderStatus(Constant.REPLACE_REJECTED);
		} else {
			orders.setOrderStatus(Constant.REJECTED);
		}
		TicketReason ticketReason = ticketReasonService.getTicketReasonDetails(replaceCancelOrderDto.getReasonId());
		orders.setReturnReplaceReason(ticketReason);
		orders.setCancelReturnReplaceDescription(replaceCancelOrderDto.getDescription());
		orders.setRefunded(true);
		ordersRepository.save(orders);
		saveOrderStatusHistory(orders);

		/**
		 * Update the wallet for the customer, incase of online payment
		 */
		if (OrderStatusEnum.PENDING.getStatusValue().equals(orders.getOrderStatus()) && !PaymentMode.COD.name().equals(orders.getPaymentMode())) {
			Double amountToBeCredited = orders.getTotalOrderAmount() + orders.getWalletContribution();
			Double existingWalletAmt = orders.getCustomer().getWalletAmt();

			customerService.updateWalletBalance(amountToBeCredited + existingWalletAmt, orders.getCustomer().getId());
			/**
			 * make an entry in wallet txn
			 */
			addWalletTxn(amountToBeCredited, orders.getCustomer().getId(), orders.getId(), null);
		}
	}

	@Override
	public List<String> getNextStatus(final Long orderId) throws NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		Orders order = getOrderById(orderId);
		OrderStatusEnum existingOrderStatus = OrderStatusEnum.getByValue(order.getOrderStatus());
		BasicStatus<OrderStatusEnum>[] nextOrderStatus = existingOrderStatus.nextStatus();

		List<String> nextStatus = new ArrayList<>();
		/**
		 * If the user is vendor
		 */
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {

			/**
			 * Vendor cannot move the order in In-Process, Delivered, Cancelled,
			 */
			List<String> statusListInWhichVendorCannotMoveOrder = new ArrayList<>();
			statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.IN_PROCESS.getStatusValue());
			statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.DELIVERED.getStatusValue());
			statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.RETURN_PROCESSED.getStatusValue());
			statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.RETURNED.getStatusValue());
			statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.REPLACE_PROCESSED.getStatusValue());
			statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.REPLACED.getStatusValue());

			/**
			 * If the order delivery type is not pick-up then the order cannot be moved into
			 * order_pickup status by vendor, that
			 * will be done by delivery boy when he picks up the order from the restaurant.
			 */
			if (!DeliveryType.PICKUP.getStatusValue().equals(order.getDeliveryType())) {
				statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.ORDER_PICKED_UP.getStatusValue());
				statusListInWhichVendorCannotMoveOrder.add(OrderStatusEnum.RETURN_ORDER_PICKUP.getStatusValue());
			}

			for (BasicStatus<OrderStatusEnum> status : nextOrderStatus) {
				nextStatus.add(status.getStatusValue());
			}
			nextStatus.removeAll(statusListInWhichVendorCannotMoveOrder);
			return nextStatus;
		}
		/**
		 * if user is customer
		 */
		else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			/**
			 * If required then add code here for customer. Currently its not required as it
			 * should be handled from front end.
			 */
			return nextStatus;
		}
		/**
		 * if user is Admin, then he can only cancel order, if the order is not in the
		 * below mentioned status. If the order is
		 * in below mentioned status he cannot do the cancellation as well.
		 */
		else {

			if (!Arrays
					.asList(OrderStatusEnum.REJECTED.getStatusValue(), OrderStatusEnum.DELIVERED.getStatusValue(), OrderStatusEnum.CANCELLED.getStatusValue(),
							OrderStatusEnum.REPLACED.getStatusValue(), OrderStatusEnum.RETURNED.getStatusValue())
					.contains(existingOrderStatus.getStatusValue())) {
				nextStatus.add(OrderStatusEnum.CANCELLED.getStatusValue());
			}
			return nextStatus;
		}

	}

	@Override
	public void retryForSearchingDeliveryBoys(final Long orderId) throws ValidationException, NotFoundException {
		Orders orders = getOrderById(orderId);
		if (!OrderStatusEnum.CONFIRMED.getStatusValue().equals(orders.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("order.already.accepted", null));
		} else {
			orders.setAssignmentTryCount(0);
			ordersRepository.save(orders);
			/**
			 * remove delivery boy notification history for this order
			 */
			List<DeliveryBoySendNotificationHistory> deliveryBoySendNotificationHistoryList = deliveryBoySendNotificationHistoryRepository
					.findAllByOrderId(orderId);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(deliveryBoySendNotificationHistoryList)) {
				deliveryBoySendNotificationHistoryRepository.deleteAll(deliveryBoySendNotificationHistoryList);
			}
		}
	}

	@Override
	public void refundAmount(final Long orderId, final Double amount, final String description) throws NotFoundException, ValidationException {

		Orders orders = getOrderById(orderId);

		if (PaymentMode.COD.name().equals(orders.getPaymentMode())) {
			throw new ValidationException(messageByLocaleService.getMessage("COD.orders.not.refunded", null));
		}

		Double totalOrderAmount = Double.sum(orders.getTotalOrderAmount(), orders.getWalletContribution());

		if (amount.compareTo(0.0d) == 0) {
			throw new ValidationException(messageByLocaleService.getMessage("refund.amount.non.zero", new Object[] { totalOrderAmount }));
		} else if (Math.abs(amount) > totalOrderAmount.doubleValue()) {
			throw new ValidationException(messageByLocaleService.getMessage("max.refund.amount", new Object[] { totalOrderAmount }));
		}

		if (orders.getRefunded()) {
			throw new ValidationException(messageByLocaleService.getMessage("order.already.refunded", null));
		} else if (!orders.getOrderStatus().equals(OrderStatusEnum.CANCELLED.getStatusValue())) {
			throw new ValidationException(messageByLocaleService.getMessage("cancelled.orders.refunded", null));
		}
		/**
		 * this means the refund is to be made to customer wallet after deducting the
		 * delivery charges from the order
		 */
		customerService.updateWalletBalance(amount, orders.getCustomer().getId());
		/**
		 * make an entry in wallet txn
		 */
		addWalletTxn(amount, orders.getCustomer().getId(), orders.getId(), description);

		orders.setRefunded(true);
		ordersRepository.save(orders);
	}

	/**
	 * For Return order we directly delivered task after order accepted
	 */
	@Override
	public void deliverPickUpOrder(final Long orderId) throws NotFoundException, ValidationException {

		/**
		 * Validate the order first
		 */
		Orders order = getOrder(orderId);
		if (!DeliveryType.PICKUP.getStatusValue().equals(order.getDeliveryType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}

		List<Task> taskList = taskService.getTaskListForOrderId(orderId);
		/**
		 * Here the first non delivered task will be marked as delivered, this is
		 * because, there will be 2 tasks for an order
		 * only in case of Return/REplace request received, at that time the delivery
		 * task would already be delivered
		 */
		for (Task task : taskList) {
			if (!TaskStatusEnum.DELIVERED.getStatusValue().equals(task.getStatus())) {
				if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType())) {
					/**
					 * Change the task status to on the way so the order status would be pickedUp
					 */
					taskService.changeTaskStatus(task.getId(), TaskStatusEnum.ON_THE_WAY.getStatusValue());

				}
				/**
				 * Then complete the task, so the order will be delivered
				 */
				taskService.completeTask(task.getId());
				break;
			}
		}

	}

	@Override
	public OrdersResponseDTO getOngoingOrderForCustomer() throws ValidationException, NotFoundException {
		Long customerId = getCustomerIdForLoginUser();
		Long orderId = ordersRepository.getOrderIdOfOngoingOrdersForCustomer(customerId, Constant.getCompletedOrderStatusList());
		return getOrderDetails(orderId);
	}
}
