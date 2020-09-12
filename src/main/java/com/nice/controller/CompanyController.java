package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CompanyDTO;
import com.nice.dto.CompanyResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CompanyService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@RequestMapping(path = "/company")
@RestController
public class CompanyController {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */

	private static final Logger LOGGER = LoggerFactory.getLogger(CompanyController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private CompanyService companyService;

	/**
	 * add company
	 *
	 * @param  accessToken
	 * @param  logo
	 * @param  companyDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping()
	@PreAuthorize("hasPermission('Company','CAN_ADD')")
	public ResponseEntity<Object> addCompany(@RequestHeader(value = "Authorization") final String accessToken,
			@RequestParam(name = "logo", required = false) final MultipartFile logo, @ModelAttribute @Valid final CompanyDTO companyDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Company {}", companyDTO);

		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Company validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		if (logo == null) {
			LOGGER.error("Company logo required");
			throw new ValidationException(messageByLocaleService.getMessage("image.not.null", null));
		}
		companyService.addCompany(companyDTO, logo);
		LOGGER.info("Outside add Company ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("company.create.message", null))
				.create();

	}

	/**
	 * update company
	 *
	 * @param  accessToken
	 * @param  logo
	 * @param  userId
	 * @param  companyDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping()
	@PreAuthorize("hasPermission('Company','CAN_EDIT')")
	public ResponseEntity<Object> updateCompany(@RequestHeader(value = "Authorization") final String accessToken,
			@RequestParam(name = "logo", required = false) final MultipartFile logo, @ModelAttribute @Valid final CompanyDTO companyDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Company {}", companyDTO);

		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Company validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		companyService.updateCompany(companyDTO, logo);
		LOGGER.info("Outside update company ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("company.update.message", null))
				.create();

	}

	/**
	 * get company
	 *
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping()
	public ResponseEntity<Object> getCompany() throws NotFoundException {
		LOGGER.info("Inside get company method");

		final CompanyResponseDTO resultCompany = companyService.getCompany(true);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("company.detail.message", null))
				.setData(resultCompany).create();

	}

}
