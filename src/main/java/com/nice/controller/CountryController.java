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

import com.nice.dto.CountryDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CountryMapper;
import com.nice.model.Country;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CountryService;
import com.nice.validator.CountryValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@RequestMapping(path = "/country")
@RestController
public class CountryController {

	private static final Logger LOGGER = LoggerFactory.getLogger(CountryController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding country
	 */
	@Autowired
	private CountryValidator countryValidator;

	@Autowired
	private CountryMapper countryMapper;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(countryValidator);
	}

	@Autowired
	private CountryService countryService;

	/**
	 * Add Country
	 *
	 * @param  accessToken
	 * @param  countryDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Country','CAN_ADD')")
	public ResponseEntity<Object> addCountry(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final CountryDTO countryDTO,
			final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add country {}", countryDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Country validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Country resultCountry = countryService.addCountry(countryDTO);
		LOGGER.info("Outside add country {}", resultCountry);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("country.create.message", null))
				.setData(resultCountry).create();
	}

	/**
	 * Update Country
	 *
	 * @param  accessToken
	 * @param  countryDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Country','CAN_EDIT')")
	public ResponseEntity<Object> updateCountry(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final CountryDTO countryDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update country {}", countryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Country validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		final Country resultCountry = countryService.updateCountry(countryDTO);
		LOGGER.info("Outside update country {}", resultCountry);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("country.update.message", null))
				.setData(resultCountry).create();
	}

	/**
	 * Get Country Details based on id
	 *
	 * @param  countryId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{countryId}")
	public ResponseEntity<Object> getCountry(@PathVariable("countryId") final Long countryId) throws NotFoundException {
		final CountryDTO resultCountry = countryService.getCountry(countryId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("country.detail.message", null))
				.setData(resultCountry).create();
	}

	/**
	 * Get Country list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getCountryList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyWord) throws NotFoundException, ValidationException {
		final Page<Country> resultCountries = countryService.getCountryList(pageNumber, pageSize, activeRecords, searchKeyWord);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("country.list.message", null))
				.setData(countryMapper.toDtos(resultCountries.getContent())).setHasNextPage(resultCountries.hasNext())
				.setHasPreviousPage(resultCountries.hasPrevious()).setTotalPages(resultCountries.getTotalPages()).setPageNumber(resultCountries.getNumber() + 1)
				.setTotalCount(resultCountries.getTotalElements()).create();

	}

	/**
	 * Change status of country (active/deActive)
	 *
	 * @param  accessToken
	 * @param  countryId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{countryId}")
	@PreAuthorize("hasPermission('Country','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("countryId") final Long countryId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of country ofr id {} and status {}", countryId, active);
		countryService.changeStatus(countryId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("country.update.message", null))
				.create();
	}

}
