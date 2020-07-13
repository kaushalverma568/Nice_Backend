/**
 *
 */
package com.nice.controller;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import com.nice.constant.SuccessErrorType;
import com.nice.exception.NotFoundException;
import com.nice.model.Orders;
import com.nice.service.OrdersService;
import com.nice.service.PaymentService;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */

@RequestMapping(path = "/payment")
@RestController
public class PaymentController {

	@Autowired
	private PaymentService paymentService;

	@Autowired
	private OrdersService ordersService;

	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentController.class);

	@Value("${customer.url}")
	private String customerUrl;

	private static final String REDIRECT = "redirect:";

	@PostMapping(path = "/check")
	public ModelAndView checkPayment(@RequestParam(name = "razorpay_payment_id", required = false) final String razorpayPaymentId,
			@RequestParam(name = "razorpay_signature", required = false) final String razorpaySignature,
			@RequestParam(name = "razorpay_order_id", required = false) final String razorpayOrderId,
			@RequestParam(name = "error[code]", required = false) final String errorCode,
			@RequestParam(name = "error[description]", required = false) final String errorDescription,
			@RequestParam(name = "error[field]", required = false) final String errorField) throws NotFoundException {
		LOGGER.info("razorpayPaymentId : {} , razorpaySignature : {} , razorpayOrderId : {} , errorCode : {} ,errorDescription : {}", razorpayPaymentId,
				razorpaySignature, razorpayOrderId, errorCode, errorDescription);
		if ((errorCode != null) && (errorDescription != null)) {
			paymentService.failedTransaction(razorpayOrderId);
			return new ModelAndView(REDIRECT + customerUrl + "#/failed-error?message='" + errorDescription + "'&type=" + SuccessErrorType.PAYMENT);
		}
		final boolean data = paymentService.checkPaymentTransaction(razorpayOrderId, razorpayPaymentId, razorpaySignature);
		if (data) {

			final Optional<Orders> orders = ordersService.getOrderDetailsByOnlineOrderId(razorpayOrderId);
			Long orderId = 0L;
			if (orders.isPresent()) {
				orderId = orders.get().getId();
				/**
				 * send email start here
				 */
				/**
				 * send email to customer when he/she place order and payment type is online
				 */
				// ordersService.sendEmailOnOrderStatusChange(orderId,
				// NotificationQueueConstants.PLACE_ORDER);
				/**
				 * send email ends here
				 */
			}

			return new ModelAndView(REDIRECT + customerUrl + "#/thank-you?message=Payment Successful. Your order Placed Successfully&orderid=" + orderId
					+ " &type=" + SuccessErrorType.PAYMENT);
		} else {
			return new ModelAndView(REDIRECT + customerUrl + "#/failed-error?type=" + SuccessErrorType.PAYMENT);
		}
	}
}
