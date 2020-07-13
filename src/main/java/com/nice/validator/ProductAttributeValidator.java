package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.ProductAttributeDTO;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.ProductAttributeService;

@Component
public class ProductAttributeValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private ProductAttributeService productAttributeService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ProductAttributeDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final ProductAttributeDTO productAttributeDTO = (ProductAttributeDTO) target;

		try {
			if (productAttributeService.isExists(productAttributeDTO)) {
				errors.rejectValue("name", "409", messageByLocaleService.getMessage("product.attribute.name.not.unique", null));
			}
		} catch (ValidationException e) {
			errors.rejectValue("name", "409", messageByLocaleService.getMessage("unauthorized", null));
		}
	}
}