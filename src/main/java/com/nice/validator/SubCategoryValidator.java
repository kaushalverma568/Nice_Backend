package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.SubCategoryDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.SubCategoryService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Component
public class SubCategoryValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SubCategoryValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private SubCategoryService subCategoryService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return SubCategoryDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof SubCategoryDTO) {
			final SubCategoryDTO subCategoryDTO = (SubCategoryDTO) target;
			try {
				// to check SubCategory duplication
				if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(subCategoryDTO.getName()) && subCategoryDTO.getCategoryId() != null
						&& subCategoryService.isSubCategoryExists(subCategoryDTO).booleanValue()) {
					errors.rejectValue("name", "409", messageByLocaleService.getMessage("subcategory.name.not.unique", null));
				}
			} catch (NotFoundException e) {
				LOGGER.error("Category not found for id : {} ", subCategoryDTO.getCategoryId());
				errors.rejectValue("categoryId", "409",
						messageByLocaleService.getMessage("category.not.found", new Object[] { subCategoryDTO.getCategoryId() }));
			}
		} else {
			LOGGER.info("target is not instance of SubCategoryDTO");
		}
	}

}
