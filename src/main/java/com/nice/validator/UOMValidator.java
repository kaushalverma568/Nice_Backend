package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.UOMDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.UOMService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component
public class UOMValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UOMValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private UOMService uomService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return UOMDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof UOMDTO) {
			final UOMDTO uomDTO = (UOMDTO) target;
			// to check uom duplication
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(uomDTO.getMeasurement()) && uomDTO.getQuantity() != null
					&& uomService.isUOMExists(uomDTO).booleanValue()) {
				errors.rejectValue("measurement", "409", messageByLocaleService.getMessage("measurement.not.unique", null));
			}
		} else {
			LOGGER.info("target is not instance of UOMDTO");
		}
	}
}
