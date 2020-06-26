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

import com.nice.dto.RoleDTO;
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
 * @date : 26-06-2020
 */
@RequestMapping(path = "/role")
@RestController
public class RoleController {

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

	@PostMapping
	public ResponseEntity<Object> addRole(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final RoleDTO roleDto,
			final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add role {}", roleDto);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("role validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		roleService.addRole(roleDto);
		LOGGER.info("Outside add role");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.create.message", null))
				.create();

	}

	@PutMapping
	public ResponseEntity<Object> updateRole(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final RoleDTO roleDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update role {}", roleDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("role validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		roleService.updateRole(roleDTO);
		LOGGER.info("Outside update role ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.update.message", null))
				.create();
	}

	@GetMapping("/{roleId}")
	public ResponseEntity<Object> getRole(@RequestHeader("Authorization") final String accessToken, @PathVariable("roleId") final Long roleId)
			throws NotFoundException {
		RoleDTO resultres = roleService.getRole(roleId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.detail.message", null))
				.setData(resultres).create();
	}

	/**
	 * get role List
	 *
	 * @param accessToken
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @return
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getRoleList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) {
		final Page<Role> resultRes = roleService.getRoleList(pageNumber, pageSize, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.list.message", null))
				.setData(roleMapper.toDtos(resultRes.getContent())).setHasNextPage(resultRes.hasNext()).setHasPreviousPage(resultRes.hasPrevious())
				.setTotalPages(resultRes.getTotalPages()).setPageNumber(resultRes.getNumber() + 1).setTotalCount(resultRes.getTotalElements()).create();

	}

	@PutMapping("/status/{roleId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("roleId") final Long roleId,
			@RequestParam final Boolean isActive) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of role of id {} and status {}", roleId, isActive);
		roleService.changeStatus(roleId, isActive);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("role.update.message", null))
				.create();
	}

}
