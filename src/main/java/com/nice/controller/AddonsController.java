package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
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
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.AddonsDTO;
import com.nice.exception.BaseException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.AddonsMapper;
import com.nice.model.Addons;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.AddonsService;
import com.nice.validator.AddonsValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 14-Jul-2020
 */
@RequestMapping(path = "/addons")
@RestController
public class AddonsController {
	/**
	 *
	 */
	private static final String LIST_MESSAGE = "addons.list.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(AddonsController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding addons
	 */
	@Autowired
	private AddonsValidator addonsValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(addonsValidator);
	}

	@Autowired
	private AddonsService addonsService;

	@Autowired
	private AddonsMapper addonsMapper;

	/**
	 * add addons
	 *
	 * @param  accessToken
	 * @param  addonsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	// @PreAuthorize("hasPermission('Addons','CAN_ADD')")
	public ResponseEntity<Object> addAddons(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final AddonsDTO addonsDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Addons {}", addonsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Addons validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		addonsService.addAddons(addonsDTO);
		LOGGER.info("Outside add Addons ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.create.message", null))
				.create();
	}

	/**
	 * update addons
	 *
	 * @param  accessToken
	 * @param  addonsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	// @PreAuthorize("hasPermission('Addons','CAN_EDIT')")
	public ResponseEntity<Object> updateAddons(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final AddonsDTO addonsDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Addons {}", addonsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Addons validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		addonsService.updateAddons(addonsDTO);
		LOGGER.info("Outside update Addons ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.update.message", null))
				.create();
	}

	/**
	 * get addons detail by id
	 *
	 * @param  accessToken
	 * @param  addonsId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{addonsId}")
	// // @PreAuthorize("hasPermission('Addons','CAN_VIEW')")
	public ResponseEntity<Object> getAddons(@RequestHeader("Authorization") final String accessToken, @PathVariable("addonsId") final Long addonsId)
			throws NotFoundException {
		LOGGER.info("Inside get Addons ");
		final AddonsDTO resultAddonsDTO = addonsService.getAddonsDetailById(addonsId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.detail.message", null))
				.setData(resultAddonsDTO).create();
	}

	/**
	 * get list based on params
	 *
	 * @param  accessToken
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	// @PreAuthorize("hasPermission('Addons','CAN_VIEW_LIST')")
	public ResponseEntity<Object> getAddonsList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword,
			@RequestParam(name = "vendorId", required = false) final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside get Addons List ");
		final Page<Addons> resultCategories = addonsService.getAddonsList(pageNumber, pageSize, activeRecords, searchKeyword, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(LIST_MESSAGE, null))
				.setData(addonsMapper.toDtos(resultCategories.getContent())).setHasNextPage(resultCategories.hasNext())
				.setHasPreviousPage(resultCategories.hasPrevious()).setTotalPages(resultCategories.getTotalPages())
				.setPageNumber(resultCategories.getNumber() + 1).setTotalCount(resultCategories.getTotalElements()).create();
	}

	/**
	 * Change status of Addons (active/deActive)
	 *
	 * @param  addonsId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{addonsId}")
	// @PreAuthorize("hasPermission('Addons','CAN_EDIT')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("addonsId") final Long addonsId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of addons of id {} and status {}", addonsId, active);
		addonsService.changeStatus(addonsId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.update.message", null))
				.create();
	}
	
	/**
	 * 
	 * @param accessToken
	 * @param file
	 * @param httpServletResponse
	 * @return
	 * @throws BaseException
	 */
	@PostMapping(path = "/upload")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = false) final MultipartFile file, final HttpServletResponse httpServletResponse) throws BaseException {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		addonsService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.create.message", null))
				.create();
	}

}