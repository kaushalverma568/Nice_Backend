package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.PincodeDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.PincodeService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
@Component
public class PincodeValidator implements Validator {
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PincodeValidator.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;
	/**
	 * service - to implement business logic
	 */

	@Autowired
	private PincodeService pincodeService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return PincodeDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */
	@Override
	public void validate(final Object target, final Errors errors) {
		final PincodeDTO pincodeDTO = (PincodeDTO) target;
		/**
		 * to check pincode duplication
		 */
		try {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pincodeDTO.getCodeValue()) && pincodeDTO.getCityId() != null
					&& pincodeService.isPincodeExists(pincodeDTO).booleanValue()) {
				errors.rejectValue("codeValue", "409", messageByLocaleService.getMessage("pincode.name.not.unique", null));
			}
		} catch (NotFoundException e) {
			LOGGER.error("city not found for id : {} ", pincodeDTO.getCityId());
			errors.rejectValue("cityId", "409", messageByLocaleService.getMessage("city.not.found", new Object[] { pincodeDTO.getCityId() }));

		}

	}
}
