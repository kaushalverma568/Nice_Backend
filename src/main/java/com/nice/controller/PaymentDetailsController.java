package com.nice.controller;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PaymentDetailsMapper;
import com.nice.model.PaymentDetails;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.PaymentDetailsService;
import com.nice.validator.PaymentDetailsValidator;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 15-07-2020
 */
@RequestMapping(path = "/paymentdetails")
@RestController
public class PaymentDetailsController {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentDetailsController.class);
	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding paymentDetails
	 */
	@Autowired
	private PaymentDetailsValidator paymentDetailsValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(paymentDetailsValidator);
	}

	@Autowired
	private PaymentDetailsService paymentDetailsService;

	@Autowired
	private PaymentDetailsMapper paymentDetailsMapper;

	/**
	 * Add PaymentDetails
	 *
	 * @param paymentDetailsDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addPaymentDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final PaymentDetailsDTO paymentDetailsDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add PaymentDetails {}", paymentDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("PaymentDetails validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		paymentDetailsService.addPaymentDetails(paymentDetailsDTO);
		LOGGER.info("Outside add PaymentDetails ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("payment.details.create.message", null)).create();
	}

	/**
	 * Get PaymentDetails Details based on id
	 *
	 * @param paymentDetailsId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(name = "getPaymentDetails", value = "/{paymentDetailsId}")
	public ResponseEntity<Object> getPaymentDetails(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("paymentDetailsId") final Long paymentDetailsId) throws NotFoundException {
		LOGGER.info("Inside get PaymentDetails for id:{} ", paymentDetailsId);
		final PaymentDetailsResponseDTO resultPaymentDetailsDTO = paymentDetailsService.getPaymentDetails(paymentDetailsId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("payment.details.detail.message", null)).setData(resultPaymentDetailsDTO).create();
	}

	/**
	 * Get payment History
	 * 
	 * @param accessToken
	 * @param fromDate
	 * @param toDate
	 * @return
	 */
	@GetMapping("/history")
	public ResponseEntity<Object> getPaymentDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "fromDate", required = false) final Date fromDate, @RequestParam(name = "toDate", required = false) final Date toDate) {
		LOGGER.info("Inside get payment history for fromDate:{} and toDate:{} ", fromDate, toDate);
		final List<PaymentDetails> paymentHistory = paymentDetailsService.getPaymentHistory(fromDate, toDate);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("payment.details.detail.message", null)).setData(paymentDetailsMapper.toDtos(paymentHistory))
				.create();
	}
}