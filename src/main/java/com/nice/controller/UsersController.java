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

import com.nice.dto.UsersDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.UsersMapper;
import com.nice.model.Users;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.UsersService;
import com.nice.validator.UsersValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@RestController
@RequestMapping(value = "/users")
public class UsersController {

	private static final Logger LOGGER = LoggerFactory.getLogger(UsersController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding users
	 */
	@Autowired
	private UsersValidator usersValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(usersValidator);
	}

	@Autowired
	private UsersService usersService;

	@Autowired
	private UsersMapper usersMapper;

	/**
	 * Add Users
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  usersDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addUsers(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final UsersDTO usersDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add users {}", usersDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Users validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Users resultUsers = usersService.addUsers(usersDTO);
		LOGGER.info("Outside add users {}", resultUsers);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("users.create.message", null))
				.setData(resultUsers).create();
	}

	/**
	 * Update Users
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  usersDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updateUsers(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final UsersDTO usersDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update users {}", usersDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Users validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		final Users resultUsers = usersService.updateUsers(usersDTO);
		LOGGER.info("Outside update users {}", resultUsers);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("users.update.message", null))
				.setData(resultUsers).create();
	}

	/**
	 * Get Users Details based on id
	 *
	 * @param  usersId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{usersId}")
	public ResponseEntity<Object> getUsers(@PathVariable("usersId") final Long usersId) throws NotFoundException, ValidationException {
		final UsersDTO resultUsers = usersService.getUsers(usersId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("users.detail.message", null))
				.setData(resultUsers).create();
	}

	/**
	 * Get Users list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getUsersList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) {
		final Page<Users> usersList = usersService.getUsersList(pageNumber, pageSize, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("users.list.message", null))
				.setData(usersMapper.toDtos(usersList.getContent())).setHasNextPage(usersList.hasNext()).setHasPreviousPage(usersList.hasPrevious())
				.setTotalPages(usersList.getTotalPages()).setPageNumber(usersList.getNumber() + 1).setTotalCount(usersList.getTotalElements()).create();

	}

	/**
	 * Change status of users (active/deActive)
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  usersId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{usersId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("usersId") final Long usersId,
			@RequestParam("active") final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of users ofr id {} and status {}", usersId, active);
		usersService.changeStatus(usersId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("users.update.message", null))
				.create();
	}

}
