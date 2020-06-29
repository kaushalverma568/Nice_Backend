package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.CuisineDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CuisineService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 31-Dec-2019
 */
@Component
public class CuisineValidator implements Validator {

	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
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
	 * purpose - to validate object and apply various validations. this method may
	 * carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final CuisineDTO cuisineDTO = (CuisineDTO) target;
		/**
		 * Check cuisine duplication based on name
		 */
		if (cuisineDTO != null && cuisineDTO.getName() != null && cuisineService.isCuisineExists(cuisineDTO)) {
			errors.rejectValue("name", "409", messageByLocaleService.getMessage("cuisine.name.not.unique", null));
		}

	}

}
