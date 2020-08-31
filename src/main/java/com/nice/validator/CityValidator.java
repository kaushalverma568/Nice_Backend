package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.CityDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CityService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Component
public class CityValidator implements Validator {
	private static final Logger LOGGER = LoggerFactory.getLogger(CityValidator.class);
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
	private CityService cityService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return CityDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may
	 * carry number of validation conditions.
	 */
	@Override
	public void validate(final Object target, final Errors errors) {
		final CityDTO cityDTO = (CityDTO) target;

		/**
		 * Added to handle the State in this, default it will be 1
		 */
		cityDTO.setStateId(1L);

		/**
		 * Addition for state ends
		 */

		try {
			/**
			 * to check city duplication
			 */
			if (cityDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(cityDTO.getNameEnglish())
					&& CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(cityDTO.getNameArabic()) && cityDTO.getStateId() != null
					&& cityService.isCityExists(cityDTO)) {
				errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("city.name.not.unique", null));
			}
		} catch (NotFoundException e) {
			LOGGER.error("State not found for id : {} ", cityDTO.getStateId());
			errors.rejectValue("stateId", "409", messageByLocaleService.getMessage("state.not.found", new Object[] { cityDTO.getStateId() }));
		}
	}
}
