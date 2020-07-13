package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.CategoryDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CategoryService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class CategoryValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CategoryValidator.class);

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
	private CategoryService categoryService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return CategoryDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may
	 * carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof CategoryDTO) {
			final CategoryDTO categoryDTO = (CategoryDTO) target;
			// to check category duplication
			try {
				if (categoryDTO.getVendorId() != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(categoryDTO.getName())
						&& categoryService.isCategoryExists(categoryDTO).booleanValue()) {
					errors.rejectValue("name", "409", messageByLocaleService.getMessage("category.name.not.unique", null));
				}
			} catch (NotFoundException e) {
				LOGGER.error("Category not found for id : {} ", categoryDTO.getVendorId());
				errors.rejectValue("vendorId", "409", messageByLocaleService.getMessage("vendor.not.found", new Object[] { categoryDTO.getVendorId() }));

			}
		} else {
			LOGGER.info("target is not instance of CategoryDTO");
		}
	}
}
