package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.RoleAndPermissionResponseDTO;
import com.nice.dto.RoleAndPermissionsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.RoleMapper;
import com.nice.model.Role;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.RoleService;
import com.nice.validator.RoleValidator;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@RequestMapping(path = "/role")
@RestController
public class RoleController {

	/**
	 *
	 */
	private static final String ROLE_VALIDATION_FAILED = "role validation failed";
	private static final Logger LOGGER = LoggerFactory.getLogger(RoleController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	/**
	 * service - to implement business logic
	 */
	@Autowired
	private RoleService roleService;

	@Autowired
	private RoleValidator roleValidator;

	@Autowired
	private RoleMapper roleMapper;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(roleValidator);
	}

	/**
	 * add/update role with permissions
	 *
	 * @param  accessToken
	 * @param  roleAndPermissinsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/permission")
	@PreAuthorize("hasPermission('Role-Permission','CAN_ADD')")
	public ResponseEntity<Object> addUpdateRoleWithPermissions(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final RoleAndPermissionsDTO roleAndPermissinsDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add role with permissions {}", roleAndPermissinsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(ROLE_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		roleService.addUpdateRoleWithPermissions(roleAndPermissinsDTO);
		LOGGER.info("Outside add role with permissions");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.create.message", null))
				.create();
	}

	/**
	 * get role with permissions
	 *
	 * @param  accessToken
	 * @param  roleId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/permission/{roleId}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_VIEW')")
	public ResponseEntity<Object> getRoleWithPermissions(@RequestHeader("Authorization") final String accessToken, @PathVariable("roleId") final Long roleId)
			throws NotFoundException {
		RoleAndPermissionResponseDTO roleAndPermissionResponseDTO = roleService.getRoleDetailWithPermission(roleId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.detail.message", null))
				.setData(roleAndPermissionResponseDTO).create();
	}

	/**
	 * get role List
	 *
	 * @param  accessToken
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_VIEW')")
	public ResponseEntity<Object> getRoleList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "isDefault", required = false) final Boolean isDefault) {
		final Page<Role> resultRes = roleService.getRoleList(pageNumber, pageSize, activeRecords, isDefault);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.list.message", null))
				.setData(roleMapper.toDtos(resultRes.getContent())).setHasNextPage(resultRes.hasNext()).setHasPreviousPage(resultRes.hasPrevious())
				.setTotalPages(resultRes.getTotalPages()).setPageNumber(resultRes.getNumber() + 1).setTotalCount(resultRes.getTotalElements()).create();

	}

	/**
	 * Delete role
	 *
	 * @param  accessToken
	 * @param  roleId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@DeleteMapping("/{roleId}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_DELETE')")
	public ResponseEntity<Object> deleteRole(@RequestHeader("Authorization") final String accessToken, @PathVariable("roleId") final Long roleId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside delete role of id {}", roleId);
		roleService.deleteRole(roleId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.delete.message", null))
				.create();
	}
}
