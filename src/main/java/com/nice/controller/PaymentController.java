/**
 *
 */
package com.nice.controller;

import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import com.google.gson.Gson;
import com.nice.constant.NotificationQueueConstants;
import com.nice.dto.HesabeDecryptPaymentDTO;
import com.nice.dto.HesabePaymentResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.HesabePaymentService;
import com.nice.service.OrdersService;
import com.nice.service.PaymentService;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 13-07-2020
 */

@RequestMapping(path = "/payment")
@RestController
public class PaymentController {

	@Autowired
	private PaymentService paymentService;

	@Autowired
	private OrdersService ordersService;

	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentController.class);

	@Value("${static.url}")
	private String staticUrl;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private HesabePaymentService hesabePaymentService;

	private static final String REDIRECT = "redirect:";

	@GetMapping(path = "/check")
	public ModelAndView checkPayment(@RequestParam(name = "language", required = true) final String langauge, @RequestParam(name = "data") final String data)
			throws NotFoundException {
		Locale locale = new Locale(langauge);
		LocaleContextHolder.setLocale(locale);
		String result = hesabePaymentService.decrypt(data);
		LOGGER.info("hesabe response {} ", result);
		Gson gson = new Gson();
		HesabePaymentResponseDTO hesabePaymentResponseDTO = gson.fromJson(result, HesabePaymentResponseDTO.class);
		HesabeDecryptPaymentDTO decryptPaymentDTO = gson.fromJson(hesabePaymentResponseDTO.getResponse().get("data"), HesabeDecryptPaymentDTO.class);
		Long orderId;
		String msg;
		try {
			orderId = paymentService.checkPaymentTransaction(decryptPaymentDTO.getResponse());
			msg = messageByLocaleService.getMessage("payment.success", null);
			/**
			 * send push notification
			 */
			if (orderId != 0) {
				ordersService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.NEW_ORDER_PUSH_NOTIFICATION, orderId);
				ordersService.sendPushNotificationForOrder(NotificationQueueConstants.PLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER, Long.valueOf(orderId));
			} else {
				/**
				 * If the payment failure push notificaiton is to be sent in future
				 */
				msg = messageByLocaleService.getMessage("payment.error", null);
			}
		} catch (NotFoundException | ValidationException e) {
			orderId = 0l;
			msg = e.getMessage();
		}
		if (orderId.compareTo(0l) != 0) {
			return new ModelAndView(REDIRECT + staticUrl + "success");
		} else {
			return new ModelAndView(REDIRECT + staticUrl + "error");
		}
	}
}
