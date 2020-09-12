/**
 *
 */
package com.nice.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import com.google.gson.Gson;
import com.nice.constant.SuccessErrorType;
import com.nice.dto.HesabeDecryptPaymentDTO;
import com.nice.dto.HesabePaymentResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.HesabePaymentService;
import com.nice.service.PaymentService;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 13-07-2020
 */

@RequestMapping(path = "/payment")
@RestController
public class PaymentController {

	@Autowired
	private PaymentService paymentService;

	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentController.class);

	@Value("${admin.url}")
	private String adminUrl;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private HesabePaymentService hesabePaymentService;

	private static final String REDIRECT = "redirect:";

	@GetMapping(path = "/check")
	public ModelAndView checkPayment(@RequestParam(name = "data") final String data) throws NotFoundException {
		String result = hesabePaymentService.decrypt(data);
		LOGGER.info("hesabe response {} ", result);
		Gson gson = new Gson();
		HesabePaymentResponseDTO hesabePaymentResponseDTO = gson.fromJson(result, HesabePaymentResponseDTO.class);
		HesabeDecryptPaymentDTO decryptPaymentDTO = gson.fromJson(hesabePaymentResponseDTO.getResponse().get("data"), HesabeDecryptPaymentDTO.class);
		boolean response;
		String msg;
		try {
			response = paymentService.checkPaymentTransaction(decryptPaymentDTO.getResponse());
			msg = messageByLocaleService.getMessage("payment.success", null);
			// TODO send email
		} catch (NotFoundException | ValidationException e) {
			response = false;
			msg = e.getMessage();
		}
		if (response) {
			return new ModelAndView(REDIRECT + adminUrl + "auth/thank-you?message=" + msg + " &type=" + SuccessErrorType.PAYMENT);
		} else {
			return new ModelAndView(REDIRECT + adminUrl + "auth/error?message=" + msg + " &type=" + SuccessErrorType.PAYMENT);
		}
	}
}
