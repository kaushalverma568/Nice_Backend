package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.constant.Constant;
import com.nice.dto.StateDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.StateService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 13-Jan-2020
 */
@Component
public class StockValidator implements Validator {
	private static final Logger LOGGER = LoggerFactory.getLogger(StockValidator.class);
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
	private StateService stateService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return StateDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may
	 * carry number of validation conditions.
	 */
	@Override
	public void validate(final Object target, final Errors errors) {
		final StateDTO stateDTO = (StateDTO) target;

		try {
			/**
			 * to check State duplication
			 */
			if (stateDTO != null && stateDTO.getName() != null && stateDTO.getCountryId() != null && stateService.isStateExists(stateDTO)) {
				errors.rejectValue("name", "409", messageByLocaleService.getMessage("name.not.unique", new Object[] { Constant.STATE }));
			}
		} catch (NotFoundException e) {
			LOGGER.error("Country not found for id : {} ", stateDTO.getCountryId());
			errors.rejectValue("countryId", "409", messageByLocaleService.getMessage("not.found", new Object[] { Constant.COUNTRY, stateDTO.getCountryId() }));
		}
	}
}
