package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.PayableAmountDTO;
import com.nice.dto.PaymentDetailsDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.PaymentDetailsService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Component
public class PaymentDetailsValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentDetailsValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private PaymentDetailsService paymentDetailsService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return PaymentDetailsDTO.class.equals(clazz) || PayableAmountDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof PaymentDetailsDTO) {
			final PaymentDetailsDTO paymentDetailsDTO = (PaymentDetailsDTO) target;
			// to check paymentDetails duplication
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(paymentDetailsDTO.getTransactionNo())
					&& paymentDetailsService.isPaymentDetailsExists(paymentDetailsDTO).booleanValue()) {
				errors.rejectValue("transactionNo", "409", messageByLocaleService.getMessage("transactionNo.not.unique", null));
			}
		} else {
			LOGGER.info("target is not instance of PaymentDetailsDTO");
		}
	}
}
