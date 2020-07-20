package com.nice.controller;

import java.io.IOException;
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
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.UOMDTO;
import com.nice.exception.BaseException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.UOMMapper;
import com.nice.model.UOM;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.UOMService;
import com.nice.validator.UOMValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@RequestMapping(path = "/uom")
@RestController
public class UOMController {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UOMController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UOMService uomService;

	@Autowired
	private UOMMapper uomMapper;

	@Autowired
	private UOMValidator uomValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(uomValidator);
	}

	/**
	 * Add UOM
	 *
	 * @param uomDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addUOM(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final UOMDTO uomDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add UOM {}", uomDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("UOM validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		uomService.addUOM(uomDTO);
		LOGGER.info("Outside add UOM ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.create.message", null))
				.create();
	}

	/**
	 * update UOM
	 * 
	 * @param uomDTO
	 * @param result
	 *
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updateUOM(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final UOMDTO uomDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update UOM {}", uomDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("UOM validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		uomService.updateUOM(uomDTO);
		LOGGER.info("Outside update UOM ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.update.message", null))
				.create();
	}

	/**
	 * Get UOM Details based on id
	 *
	 * @param uomId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(name = "getUOM", value = "/{uomId}")
	public ResponseEntity<Object> getUOM(@RequestHeader("Authorization") final String accessToken, @PathVariable("uomId") final Long uomId)
			throws NotFoundException {
		LOGGER.info("Inside get UOM ");
		final UOMDTO resultUOMDTO = uomService.getUOM(uomId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.detail.message", null))
				.setData(resultUOMDTO).create();
	}

	/**
	 * Get UOM list
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(name = "getUOMList", value = "/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getUOMList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "vendorId", required = false) final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside get UOM List ");
		final Page<UOM> resultUOMs = uomService.getUOMList(pageNumber, pageSize, activeRecords, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.list.message", null))
				.setData(uomMapper.toDtos(resultUOMs.getContent())).setHasNextPage(resultUOMs.hasNext()).setHasPreviousPage(resultUOMs.hasPrevious())
				.setTotalPages(resultUOMs.getTotalPages()).setPageNumber(resultUOMs.getNumber() + 1).setTotalCount(resultUOMs.getTotalElements()).create();
	}

	/**
	 * Change status of UOM (active/deActive)
	 *
	 * @param uomId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping(name = "changeStatus", value = "/status/{uomId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("uomId") final Long uomId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of uom of id {} and status {}", uomId, active);
		uomService.changeStatus(uomId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.update.message", null))
				.create();
	}
	
	
	@GetMapping("/export/list")
	public ResponseEntity<Object> exportList(@RequestHeader("Authorization") final String accessToken, 
			final HttpServletResponse httpServletResponse, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords)
			throws IOException {
		uomService.exportList(activeRecords, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.list.message", null))
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
		uomService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("uom.create.message", null))
				.create();
	}

}