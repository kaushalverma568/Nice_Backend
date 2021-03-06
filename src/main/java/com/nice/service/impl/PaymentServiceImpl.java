/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.CartItemStatus;
import com.nice.constant.Constant;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.PaymentMode;
import com.nice.dto.HesabePaymentDTO;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.PushNotificationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.model.CartItem;
import com.nice.model.OnlineCart;
import com.nice.model.Orders;
import com.nice.repository.OnlineCartRepository;
import com.nice.service.CartItemService;
import com.nice.service.OrdersService;
import com.nice.service.PaymentService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 18-Feb-2020
 */
@Service(value = "paymentService")
@Transactional(rollbackFor = Throwable.class)
public class PaymentServiceImpl implements PaymentService {

	@Autowired
	private OrdersService orderService;

	@Autowired
	private OnlineCartRepository onlineCartRepository;

	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentServiceImpl.class);

	@Override
	public Long checkPaymentTransaction(final HesabePaymentDTO hesabePaymentDTO) throws NotFoundException, ValidationException {
		Long orderId = 0l;
		List<OnlineCart> onlineCartList = onlineCartRepository.findAllByOnlineOrderIdAndStatus(hesabePaymentDTO.getVariable1(),
				CartItemStatus.PAYMENT_WAITING.getStatusValue());
		if (!hesabePaymentDTO.getResultCode().equals(Constant.HESABE_CAPTURE)) {
			failedTransaction(hesabePaymentDTO.getVariable1());
			return orderId;
		}
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(onlineCartList)) {
			return orderId;
		}

		OrderRequestDTO orderRequestDTO = new OrderRequestDTO();
		List<CartItem> cartItemList = new ArrayList<>();
		Double calculatedOrderAmt = 0d;
		for (OnlineCart onlineCart : onlineCartList) {
			CartItem cartItem = new CartItem();
			/**
			 * This id will be used for getting addon,extras and topping's online list
			 */
			cartItem.setOnlineCartId(onlineCart.getId());
			cartItem.setCustomer(onlineCart.getCustomer());
			cartItem.setProductVariant(onlineCart.getProductVariant());
			cartItem.setQuantity(onlineCart.getQuantity());
			cartItemList.add(cartItem);
			orderRequestDTO.setCityId(onlineCart.getCityId());
			orderRequestDTO.setStateId(onlineCart.getStateId());
			orderRequestDTO.setAreaId(onlineCart.getAreaId());
			orderRequestDTO.setAddressEnglish(onlineCart.getAddressEnglish());
			orderRequestDTO.setAddressArabic(onlineCart.getAddressArabic());
			orderRequestDTO.setLatitude(onlineCart.getLatitude());
			orderRequestDTO.setLongitude(onlineCart.getLongitude());
			orderRequestDTO.setPhoneNumber(onlineCart.getPhoneNumber());
			orderRequestDTO.setFirstName(onlineCart.getFirstName());
			orderRequestDTO.setLastName(onlineCart.getLastName());
			orderRequestDTO.setCustomerId(onlineCart.getCustomer().getId());
			orderRequestDTO.setDeliveryType(onlineCart.getDeliveryType());
			orderRequestDTO.setPaymentMode(PaymentMode.ONLINE.name());
			orderRequestDTO.setDescription(onlineCart.getDescription());
			orderRequestDTO.setWalletContribution(onlineCart.getWalletContirbution());
			calculatedOrderAmt = onlineCart.getPaymentAmount();
		}
		orderRequestDTO.setOnlineOrderId(hesabePaymentDTO.getVariable1());
		orderRequestDTO.setPaymentToken(hesabePaymentDTO.getPaymentToken());
		orderRequestDTO.setPaymentId(hesabePaymentDTO.getPaymentId());
		orderRequestDTO.setAdministrativeCharge(hesabePaymentDTO.getAdministrativeCharge());
		Orders orders = orderService.createOrder(cartItemList, orderRequestDTO, calculatedOrderAmt);
		orderId = orders.getId();
		for (OnlineCart onlineCart : onlineCartList) {
			onlineCart.setPaymentId(hesabePaymentDTO.getPaymentId());
			onlineCart.setPaymentToken(hesabePaymentDTO.getPaymentToken());
			onlineCart.setAdministrativeCharge(hesabePaymentDTO.getAdministrativeCharge());
			onlineCart.setStatus(CartItemStatus.PAYMENT_SUCCESS.getStatusValue());
			onlineCartRepository.save(onlineCart);
		}
		/**
		 * Delete the entry from cart for the successful orders
		 */
		cartItemService.deleteCartItemForOnlineOrderId(hesabePaymentDTO.getVariable1());
		return orderId;
	}

	@Override
	public void failedTransaction(final String razorPayOrderId) {
		LOGGER.info("Inside Failed Payment transaction for razorPayOrderId {} :", razorPayOrderId);
		List<OnlineCart> onlinePaymentCartList = onlineCartRepository.findAllByOnlineOrderIdAndStatus(razorPayOrderId,
				CartItemStatus.PAYMENT_WAITING.getStatusValue());
		for (OnlineCart onlineCart : onlinePaymentCartList) {
			onlineCart.setStatus(CartItemStatus.PAYMENT_FAIL.getStatusValue());
			onlineCartRepository.save(onlineCart);
		}
	}

	@Override
	public void sendPushNotificationForPlacedOrder(final String orderPushNotificationCustomer, final Long orderId) throws NotFoundException {
		Orders orders = orderService.getOrderById(orderId);
		PushNotificationDTO pushNotificationDTO = new PushNotificationDTO();
		pushNotificationDTO.setModule(Constant.ORDER_MODULE);
		pushNotificationDTO.setOrderId(orderId);
		pushNotificationDTO.setCustomerId(orders.getCustomer().getId());
		pushNotificationDTO.setType(NotificationQueueConstants.PLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER);
		jmsQueuerService.sendPushNotification(NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE, pushNotificationDTO);
	}

}
