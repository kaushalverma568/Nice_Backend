package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.StateDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.StateService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class StateValidator implements Validator {
	private static final Logger LOGGER = LoggerFactory.getLogger(StateValidator.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
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
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */
	@Override
	public void validate(final Object target, final Errors errors) {
		final StateDTO stateDTO = (StateDTO) target;
		/**
		 * Set the countryId to 1 hardcoded
		 */
		stateDTO.setCountryId(1L);
		try {
			/**
			 * to check State duplication
			 */
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(stateDTO.getNameEnglish()) && stateDTO.getCountryId() != null
					&& stateService.isStateExistsEnglish(stateDTO)) {
				errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("state.name.english.not.unique", null));
			}
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(stateDTO.getNameArabic()) && stateDTO.getCountryId() != null
					&& stateService.isStateExistsArabic(stateDTO)) {
				errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("state.name.arabic.not.unique", null));
			}
		} catch (NotFoundException e) {
			LOGGER.error("Country not found for id : {} ", stateDTO.getCountryId());
			errors.rejectValue("countryId", "409", messageByLocaleService.getMessage("country.not.found", new Object[] { stateDTO.getCountryId() }));
		}
	}
}
