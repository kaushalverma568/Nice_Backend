package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.ProductExtrasMasterService;

@Component
public class ProductExtrasMasterValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private ProductExtrasMasterService productExtrasMasterService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ProductExtrasMasterDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final ProductExtrasMasterDTO productExtrasMasterDTO = (ProductExtrasMasterDTO) target;

		if (productExtrasMasterService.isExistsEnglish(productExtrasMasterDTO)) {
			errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("english.name.not.unique", null));
		} else if (productExtrasMasterService.isExistsArabic(productExtrasMasterDTO)) {
			errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("arabic.name.not.unique", null));
		}
	}
}