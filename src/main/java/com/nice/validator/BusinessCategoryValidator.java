package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.BusinessCategoryDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.BusinessCategoryService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Component
public class BusinessCategoryValidator implements Validator {

	/**
	 * Locale message service - to display response messages from
	 * messages_en.properties
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

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(businessCategoryDTO.getNameEnglish())
				&& businessCategoryService.isExistsEnglish(businessCategoryDTO)) {
			errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("english.name.not.unique", null));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(businessCategoryDTO.getNameArabic()) && businessCategoryService.isExistsArabic(businessCategoryDTO)) {
			errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("arabic.name.not.unique", null));
		}
	}
}