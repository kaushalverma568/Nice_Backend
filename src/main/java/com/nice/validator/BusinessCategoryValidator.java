package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.BusinessCategoryDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.BusinessCategoryService;
@Component
public class BusinessCategoryValidator  implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private BusinessCategoryService businessCategoryService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return BusinessCategoryDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final BusinessCategoryDTO businessCategoryDTO = (BusinessCategoryDTO) target;

		if ( businessCategoryService.isExists(businessCategoryDTO)) {
			errors.rejectValue("name", "409", messageByLocaleService.getMessage("business.category.name.not.unique", null));
		}
	}
}