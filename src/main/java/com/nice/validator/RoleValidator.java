/**
 *
 */
package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.RoleDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.RoleService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Component
public class RoleValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private RoleService roleService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return RoleDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final RoleDTO roleDto = (RoleDTO) target;
		/**
		 * Check country duplication based on name
		 */
		if (roleDto != null && roleDto.getName() != null && roleService.isExists(roleDto)) {
			errors.rejectValue("name", "409", messageByLocaleService.getMessage("role.name.not.unique", null));
		}

	}
}
