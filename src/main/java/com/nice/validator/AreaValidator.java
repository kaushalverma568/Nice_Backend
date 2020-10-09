package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.AreaDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.AreaService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@Component
public class AreaValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private AreaService areaService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return AreaDTO.class.equals(clazz);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(AreaValidator.class);

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final AreaDTO areaDTO = (AreaDTO) target;
		/**
		 * Check area duplication based on name
		 */
		/**
		 * added to handle default city
		 */
		if (areaDTO.getCityId() == null) {
			areaDTO.setCityId(1L);
		}
		try {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(areaDTO.getNameEnglish()) && areaService.isAreaExistsEnglish(areaDTO)) {
				errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("area.name.english.not.unique", null));
			}
		} catch (NotFoundException e) {
			LOGGER.error("city not found for id : {} ", areaDTO.getCityId());
			errors.rejectValue("cityId", "409", messageByLocaleService.getMessage("city.not.found", new Object[] { areaDTO.getCityId() }));
		}

		try {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(areaDTO.getNameArabic()) && areaService.isAreaExistsArabic(areaDTO)) {
				errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("area.name.arabic.not.unique", null));
			}
		} catch (NotFoundException e) {
			LOGGER.error("city not found for id : {} ", areaDTO.getCityId());
			errors.rejectValue("cityId", "409", messageByLocaleService.getMessage("city.not.found", new Object[] { areaDTO.getCityId() }));
		}
	}

}
