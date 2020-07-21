package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.StockTransferDto;
import com.nice.locale.MessageByLocaleService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Component
public class StockTransferValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return StockTransferDto.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final StockTransferDto stockTransferDto = (StockTransferDto) target;
		/**
		 * Validate the AddStockObject
		 */
		if (stockTransferDto.getQuantity() <= 0) {
			errors.rejectValue("quantity", "409", messageByLocaleService.getMessage("stock.transfer.quantity.non.negative", new Object[] {}));
		}

	}

}
