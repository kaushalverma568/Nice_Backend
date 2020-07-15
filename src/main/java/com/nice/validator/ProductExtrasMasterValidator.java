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

			if (productExtrasMasterService.isExists(productExtrasMasterDTO)) {
				errors.rejectValue("name", "409", messageByLocaleService.getMessage("product.extras.master.name.not.unique", null));
			}
	}
}