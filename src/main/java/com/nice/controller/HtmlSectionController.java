package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.HtmlSectionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.HtmlSectionService;
import com.nice.validator.HtmlSectionValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jan-2020
 */

@RequestMapping(path = "/sections")
@RestController
public class HtmlSectionController {

	private static final Logger LOGGER = LoggerFactory.getLogger(HtmlSectionController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private HtmlSectionService sectionService;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private HtmlSectionValidator sectionValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(sectionValidator);
	}

	/**
	 * add section
	 *
	 * @param userId
	 * @param readyMadeSizeDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping
	public ResponseEntity<Object> addText(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final HtmlSectionDTO sectionDTO,
			final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add Text of section {}", sectionDTO);

		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("section validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		sectionService.addText(sectionDTO);
		LOGGER.info("Outside add Text of section ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("section.create.message", null))
				.create();
	}

	/**
	 * update section
	 *
	 * @param userId
	 * @param sectionDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updateText(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final HtmlSectionDTO sectionDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Text of section {}", sectionDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("section validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		sectionService.updateText(sectionDTO);
		LOGGER.info("Outside update Text of section");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("section.update.message", null))
				.create();
	}

	/**
	 * get section
	 *
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping
	public ResponseEntity<Object> getText(@RequestParam(name = "type", required = true) final String type) throws NotFoundException {
		HtmlSectionDTO resultsize = sectionService.getText(type);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("section.detail.message", null))
				.setData(resultsize).create();
	}
}