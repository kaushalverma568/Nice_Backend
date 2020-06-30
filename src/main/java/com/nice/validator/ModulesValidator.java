/**
 *
 */
package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.ModulesDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.ModulesService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Component
public class ModulesValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private ModulesService modulesService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ModulesDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final ModulesDTO modulesDto = (ModulesDTO) target;
		/**
		 * Check country duplication based on name
		 */
		if (modulesDto != null && modulesDto.getName() != null && modulesService.isExists(modulesDto)) {
			errors.rejectValue("name", "409", messageByLocaleService.getMessage("modules.name.not.unique", null));
		}

	}
}
