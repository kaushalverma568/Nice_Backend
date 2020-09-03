package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.CuisineDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CuisineService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 31-Dec-2019
 */
@Component
public class CuisineValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private CuisineService cuisineService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return CuisineDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final CuisineDTO cuisineDTO = (CuisineDTO) target;
		/**
		 * Check cuisine duplication based on name
		 */
		if (cuisineDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(cuisineDTO.getNameEnglish())
				&& cuisineService.isCuisineExistsEnglish(cuisineDTO)) {
			errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("cuisine.name.english.not.unique", null));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(cuisineDTO.getNameArabic()) && cuisineService.isCuisineExistsArabic(cuisineDTO)) {
			errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("cuisine.name.arabic.not.unique", null));
		}

	}

}
