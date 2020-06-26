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

import com.nice.dto.ModulesDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ModulesMapper;
import com.nice.model.Modules;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ModulesService;
import com.nice.validator.ModulesValidator;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@RequestMapping(path = "/module")
@RestController
public class ModulesController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ModulesController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	/**
	 * service - to implement business logic
	 */
	@Autowired
	private ModulesService modulesService;

	@Autowired
	private ModulesValidator modulesValidator;

	@Autowired
	private ModulesMapper modulesMapper;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(modulesValidator);
	}

	/**
	 * add module
	 *
	 * @param accessToken
	 * @param moduleDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping()
	public ResponseEntity<Object> addModule(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final ModulesDTO moduleDTO,
			final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add module {}", moduleDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("module validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		modulesService.addModule(moduleDTO);
		LOGGER.info("Outside add module ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("modules.create.message", null))
				.create();
	}

	/**
	 * update module
	 *
	 * @param accessToken
	 * @param moduleDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping()
	public ResponseEntity<Object> updateModule(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final ModulesDTO moduleDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update module {}", moduleDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("module validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		modulesService.updateModule(moduleDTO);
		LOGGER.info("Outside update module");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("modules.update.message", null))
				.create();
	}

	/**
	 *
	 * @param accessToken
	 * @param moduleId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	@GetMapping(value = "/{moduleId}")
	public ResponseEntity<Object> getModule(@RequestHeader("Authorization") final String accessToken, @PathVariable("moduleId") final Long moduleId)
			throws NotFoundException {
		ModulesDTO resultres = modulesService.getModule(moduleId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("modules.detail.message", null))
				.setData(resultres).create();
	}

	/**
	 * get module list
	 *
	 * @param accessToken
	 * @param pageNumber
	 * @param pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getModuleList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) {
		final Page<Modules> resultRes = modulesService.getModuleList(pageNumber, pageSize, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("modules.list.message", null))
				.setData(modulesMapper.toDTOs(resultRes.getContent())).setHasNextPage(resultRes.hasNext()).setHasPreviousPage(resultRes.hasPrevious())
				.setTotalPages(resultRes.getTotalPages()).setPageNumber(resultRes.getNumber() + 1).setTotalCount(resultRes.getTotalElements()).create();

	}

	/**
	 * update status
	 *
	 * @param accessToken
	 * @param userId
	 * @param permissionId
	 * @param isActive
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/status/{moduleId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("moduleId") final Long moduleId,
			@RequestParam final Boolean isActive) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of module of id {} and status {}", moduleId, isActive);
		modulesService.changeStatus(moduleId, isActive);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("modules.update.message", null))
				.create();
	}
}
