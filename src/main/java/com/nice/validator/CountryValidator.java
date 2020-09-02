package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.CountryDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CountryService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class CountryValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private CountryService countryService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return CountryDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final CountryDTO countryDTO = (CountryDTO) target;
		/**
		 * Check country duplication based on name
		 */
		if (countryDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(countryDTO.getNameEnglish())
				&& countryService.isCountryExistsEnglish(countryDTO)) {
			errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("country.name.english.not.unique", null));
		}

		if (countryDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(countryDTO.getNameArabic())
				&& countryService.isCountryExistsArabic(countryDTO)) {
			errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("country.name.arabic.not.unique", null));
		}

	}

}
