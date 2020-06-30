/**
 *
 */
package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.PermissionDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Modules;
import com.nice.model.Role;
import com.nice.service.ModulesService;
import com.nice.service.PermissionService;
import com.nice.service.RoleService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Component
public class PermissionValidator implements Validator {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private PermissionService permissionService;

	@Autowired
	private ModulesService moduleService;

	@Autowired
	RoleService roleService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return PermissionDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		LOGGER.info("inside validate method");
		final PermissionDTO permissionDTO = (PermissionDTO) target;
		Modules module = null;
		Role role = null;
		try {
			if (permissionDTO.getModulesId() != null) {
				module = moduleService.getModuleDetail(permissionDTO.getModulesId());
			}
		} catch (NotFoundException e1) {
			errors.rejectValue("modulesId", "409", messageByLocaleService.getMessage("modules.not.found", new Object[] { permissionDTO.getModulesId() }));
		}
		try {
			if (permissionDTO.getRoleId() != null) {
				role = roleService.getRoleDetail(permissionDTO.getRoleId());
			}
		} catch (NotFoundException e1) {
			errors.rejectValue("roleId", "409", messageByLocaleService.getMessage("role.not.found", new Object[] { permissionDTO.getRoleId() }));
		}

		/**
		 * check for permission duplication
		 */
		if (permissionService.isExists(permissionDTO, module, role)) {
			errors.rejectValue("id", "409", messageByLocaleService.getMessage("permission.not.unique", null));
		}
	}
}