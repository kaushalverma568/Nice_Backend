package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.EmailUpdateDTO;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 25, 2020
 */
@Component
public class VendorValidator implements Validator {
	/**
	 *
	 */
	private static final String EMAIL = "email";

	/**
	 *
	 */
	private static final String VENDOR_EMAIL_NOT_UNIQUE = "vendor.email.not.unique";

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(VendorValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private VendorService vendorService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return VendorDTO.class.equals(clazz) || VendorRestaurantDetailsDTO.class.equals(clazz) || VendorBankDetailsDTO.class.equals(clazz)
				|| VendorFilterDTO.class.equals(clazz) || VendorListFilterDTO.class.equals(clazz) || EmailUpdateDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof VendorDTO) {
			final VendorDTO vendorDTO = (VendorDTO) target;
			// to check vendor duplication
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorDTO.getEmail())
					&& (vendorService.isVendorExists(vendorDTO).booleanValue() || vendorService.isUserLoginExists(vendorDTO).booleanValue())) {
				errors.rejectValue(EMAIL, "409", messageByLocaleService.getMessage(VENDOR_EMAIL_NOT_UNIQUE, null));
			}
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorDTO.getPhoneNumber()) && (vendorService.isVendorContactExists(vendorDTO).booleanValue())) {
				errors.rejectValue("phoneNumber", "409", messageByLocaleService.getMessage("vendor.contact.not.unique", null));
			}
		} else {
			LOGGER.info("target is not instance of VendorDTO");
		}
	}
}
