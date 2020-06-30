package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.UsersDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.UsersService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Component
public class UsersValidator implements Validator {

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
		if (usersDTO != null && usersService.isUserExists(usersDTO)) {
			errors.rejectValue("email", "409", messageByLocaleService.getMessage("user.email.exists", new Object[] { usersDTO.getEmail() }));
		}
	}
}