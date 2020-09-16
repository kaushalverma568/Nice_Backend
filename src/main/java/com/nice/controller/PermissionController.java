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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.PermissionDTO;
import com.nice.dto.PermissionResponseDTO;
import com.nice.dto.SideBarDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PermissionMapper;
import com.nice.model.Permission;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.PermissionService;
import com.nice.validator.PermissionValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Dec-2019
 */
@RequestMapping(path = "/permission")
@RestController
public class PermissionController {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionController.class);

	private static final String LIST_MESSAGE = "permission.list.message";

	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	/**
	 * service - to implement business logic
	 */
	@Autowired
	private PermissionService permissionService;

	@Autowired
	private PermissionValidator permissionValidator;

	@Autowired
	private PermissionMapper permissionMapper;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(permissionValidator);
	}

	/**
	 * add permission
	 *
	 * @param  accessToken
	 * @param  permissionDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Role-Permission','CAN_ADD')")
	public ResponseEntity<Object> addPermission(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final PermissionDTO permissionDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add permission {}", permissionDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("permission validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		permissionService.addPermission(permissionDTO);
		LOGGER.info("Outside add permission ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("permission.create.message", null))
				.create();
	}

	/**
	 * update permission
	 *
	 * @param  permissionDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Role-Permission','CAN_EDIT')")
	public ResponseEntity<Object> updatePermission(@RequestHeader("Authorization") @RequestBody @Valid final PermissionDTO permissionDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update permission {}", permissionDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("permission validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		permissionService.updatePermission(permissionDTO);
		LOGGER.info("Outside update permission ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("permission.update.message", null))
				.create();
	}

	/**
	 * get permission
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  permissionId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{permissionId}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_VIEW')")
	public ResponseEntity<Object> getPermission(@RequestHeader("Authorization") final String accessToken, @PathVariable("permissionId") final Long permissionId)
			throws NotFoundException {
		PermissionResponseDTO resultres = permissionService.getPermission(permissionId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("permission.detail.message", null))
				.setData(resultres).create();
	}

	/**
	 * get permission list by role and module
	 *
	 * @param  accessToken
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  roleId
	 * @param  moduleId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_VIEW')")
	public ResponseEntity<Object> getPermissionList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "roleId", required = false) final Long roleId, @RequestParam(name = "modulesId", required = false) final Long modulesId)
			throws NotFoundException {
		final Page<Permission> resultRes = permissionService.getPermissionList(pageNumber, pageSize, activeRecords, roleId, modulesId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(LIST_MESSAGE, null))
				.setData(permissionMapper.toResponseDTOs(resultRes.getContent())).setHasNextPage(resultRes.hasNext())
				.setHasPreviousPage(resultRes.hasPrevious()).setTotalPages(resultRes.getTotalPages()).setPageNumber(resultRes.getNumber() + 1)
				.setTotalCount(resultRes.getTotalElements()).create();
	}

	/**
	 * active/deactive permission
	 *
	 * @param  accessToken
	 * @param  permissionId
	 * @param  isActive
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/status/{permissionId}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("permissionId") final Long permissionId,
			@RequestParam final Boolean isActive) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of permission ofr id {} and status {}", permissionId, isActive);
		permissionService.changeStatus(permissionId, isActive);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("permission.update.message", null))
				.create();
	}

	/**
	 * Get side bar specific permission list for user
	 *
	 * @param  accessToken
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/sidebar")
	public ResponseEntity<Object> getSideBarSpectificPermissionListForUser(@RequestHeader("Authorization") final String accessToken) throws NotFoundException {
		final List<SideBarDTO> sideBarDTOList = permissionService.getSideBarSpectificPermissionListForUser();
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("permission.detail.message", null))
				.setData(sideBarDTOList).create();
	}
}
