package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.constant.Constant;
import com.nice.dto.AddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.AddonsService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Jul-2020
 */
@Component
public class AddonsValidator implements Validator {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(AddonsValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private AddonsService addonsService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return AddonsDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof AddonsDTO) {
			final AddonsDTO addonsDTO = (AddonsDTO) target;
			// to check Addons duplication
			try {
				if (addonsDTO.getVendorId() != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(addonsDTO.getNameEnglish())
						&& addonsService.isExistsEnglish(addonsDTO).booleanValue()) {
					errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("addons.not.unique", null));
				} else if (addonsDTO.getVendorId() != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(addonsDTO.getNameArabic())
						&& addonsService.isExistsArabic(addonsDTO).booleanValue()) {
					errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("addons.not.unique", null));
				}
			} catch (NotFoundException e) {
				LOGGER.error("Addons not found for vendor id : {} ", addonsDTO.getVendorId());
				errors.rejectValue("vendorId", "409", messageByLocaleService.getMessage("vendor.not.found", new Object[] { addonsDTO.getVendorId() }));
			} catch (ValidationException e) {
				LOGGER.error("current user is not vendor");
				errors.rejectValue("vendorId", "409", messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
			}
		} else {
			LOGGER.info("target is not instance of AddonsDTO");
		}
	}
}
