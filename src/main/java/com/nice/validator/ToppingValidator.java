package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.ToppingDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.ToppingService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Component
public class ToppingValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ToppingValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private ToppingService toppingService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ToppingDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof ToppingDTO) {
			final ToppingDTO toppingDTO = (ToppingDTO) target;
			// to check topping duplication

			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(toppingDTO.getNameEnglish()) && toppingService.isToppingExistsEnglish(toppingDTO).booleanValue()) {
				errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("english.name.not.unique", null));
			} else if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(toppingDTO.getNameArabic())
					&& toppingService.isToppingExistsArabic(toppingDTO).booleanValue()) {
				errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("arabic.name.not.unique", null));
			}
		} else {
			LOGGER.info("target is not instance of ToppingDTO");
		}
	}
}
