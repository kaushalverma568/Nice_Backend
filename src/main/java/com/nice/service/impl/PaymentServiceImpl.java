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
import com.nice.constant.PaymentMode;
import com.nice.dto.OrderRequestDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartItem;
import com.nice.model.OnlineCart;
import com.nice.repository.OnlineCartRepository;
import com.nice.service.CartItemService;
import com.nice.service.OrdersService;
import com.nice.service.PaymentService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 18-Feb-2020
 */
@Service(value = "paymentService")
@Transactional(rollbackFor = Throwable.class)
public class PaymentServiceImpl implements PaymentService {

	@Autowired
	private OrdersService orderService;

	@Autowired
	private OnlineCartRepository razorPayCartRepository;

	@Autowired
	private CartItemService cartItemService;

	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentServiceImpl.class);

	@Override
	public Boolean checkPaymentTransaction(final String onlineOrderId, final String razorPayPaymentId, final String razorPaySignature) {
		boolean result = false;
		List<OnlineCart> onlineCartList = null;
		onlineCartList = razorPayCartRepository.findAllByOnlineOrderIdAndStatus(onlineOrderId, CartItemStatus.PAYMENT_WAITING.getStatusValue());
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(onlineCartList)) {
			return result;
		}

		if (onlineOrderId == null || razorPayPaymentId == null || razorPaySignature == null) {
			for (OnlineCart razorPayCart : onlineCartList) {
				razorPayCart.setStatus(CartItemStatus.PAYMENT_FAIL.getStatusValue());
				razorPayCartRepository.save(razorPayCart);
			}
			return result;
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
			orderRequestDTO.setPincodeId(onlineCart.getPincodeId());
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
			calculatedOrderAmt = onlineCart.getPaymentAmount();
		}
		orderRequestDTO.setOnlineOrderId(onlineOrderId);
		orderRequestDTO.setOnlineSignature(razorPaySignature);
		orderRequestDTO.setTransactionId(razorPayPaymentId);
		try {
			orderService.createOrder(cartItemList, orderRequestDTO, calculatedOrderAmt);
			result = true;
		} catch (NotFoundException e) {
			LOGGER.info("Issue in add order {}", e.getMessage());
		} catch (ValidationException e) {
			LOGGER.info("Issue in add order {}", e.getMessage());
		}
		for (OnlineCart razorPayCart : onlineCartList) {
			razorPayCart.setStatus(CartItemStatus.PAYMENT_SUCCESS.getStatusValue());
			razorPayCartRepository.save(razorPayCart);
		}
		/**
		 * Delete the entry from cart for the successful orders
		 */
		try {
			cartItemService.deleteCartItemForOnlineOrderId(onlineOrderId);
		} catch (NotFoundException e) {
			LOGGER.error("Error while deleting items for cart using razorpay orderId : {}", e.getMessage());
		}

		return result;
	}

	@Override
	public void failedTransaction(final String razorPayOrderId) {
		LOGGER.info("Inside Failed Payment transaction for razorPayOrderId {} :", razorPayOrderId);
		List<OnlineCart> razorPayCartList = razorPayCartRepository.findAllByOnlineOrderIdAndStatus(razorPayOrderId,
				CartItemStatus.PAYMENT_WAITING.getStatusValue());
		for (OnlineCart razorPayCart : razorPayCartList) {
			razorPayCart.setStatus(CartItemStatus.PAYMENT_FAIL.getStatusValue());
			razorPayCartRepository.save(razorPayCart);
		}
	}

}
