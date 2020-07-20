package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.BrandDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.BrandService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Component
public class BrandValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BrandValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private BrandService brandService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return BrandDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof BrandDTO) {
			final BrandDTO brandDTO = (BrandDTO) target;
			// to check brand duplication
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(brandDTO.getName()) && brandService.isBrandExists(brandDTO).booleanValue()) {
				errors.rejectValue("name", "409", messageByLocaleService.getMessage("brand.name.not.unique", null));
			}
		} else {
			LOGGER.info("target is not instance of BrandDTO");
		}
	}
}
