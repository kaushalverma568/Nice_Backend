package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.ProductExtrasService;

@Component
public class ProductExtrasValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private ProductExtrasService productExtrasService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ProductExtrasDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final ProductExtrasDTO productExtrasDTO = (ProductExtrasDTO) target;

		try {
			if (productExtrasService.isExists(productExtrasDTO)) {
				errors.rejectValue("name", "409", messageByLocaleService.getMessage("product.extras.name.not.unique", null));
			}
		} catch (NotFoundException e) {
			errors.rejectValue("productId", "400", messageByLocaleService.getMessage("product.not.found", null));
		}
	}
}