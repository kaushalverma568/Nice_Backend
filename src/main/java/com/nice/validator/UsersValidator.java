package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.UsersDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.UsersService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Component
public class UsersValidator implements Validator {

	/**
	 *
	 */
	private static final String EMAIL = "email";

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UsersService usersService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return UsersDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final UsersDTO usersDTO = (UsersDTO) target;

		/**
		 * Check user is exists or not
		 */
		if (usersDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(usersDTO.getEmail()) && usersService.isUserExists(usersDTO)) {
			errors.rejectValue(EMAIL, "409", messageByLocaleService.getMessage("user.email.exists", new Object[] { usersDTO.getEmail() }));
		}
		/**
		 * Check user is exists or not
		 */
		if (usersDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(usersDTO.getEmail()) && usersService.isSuperAdminExists(usersDTO)) {
			errors.rejectValue(EMAIL, "409", messageByLocaleService.getMessage("user.email.exists.super.admin", new Object[] { usersDTO.getEmail() }));
		}

		/**
		 * Check user is exists or not
		 */
		if (usersDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(usersDTO.getEmail()) && usersService.isVendorExists(usersDTO)) {
			errors.rejectValue(EMAIL, "409", messageByLocaleService.getMessage("user.email.exists.vendor", new Object[] { usersDTO.getEmail() }));
		}
	}
}