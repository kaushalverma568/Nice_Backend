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

import com.nice.dto.CityDTO;
import com.nice.dto.CityResponseDTO;
import com.nice.dto.PaginationUtilDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CityMapper;
import com.nice.model.City;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CityService;
import com.nice.util.PaginationUtil;
import com.nice.validator.CityValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@RequestMapping(path = "/city")
@RestController
public class CityController {

	private static final Logger LOGGER = LoggerFactory.getLogger(CityController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding city
	 */
	@Autowired
	private CityValidator cityValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(cityValidator);
	}

	@Autowired
	private CityService cityService;

	@Autowired
	private CityMapper cityMapper;

	/**
	 * Add City
	 *
	 * @param accessToken
	 * @param cityDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('City','CAN_ADD')")
	public ResponseEntity<Object> addCity(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final CityDTO cityDTO,
			final BindingResult bindingResult) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add city {}", cityDTO);
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the State in this, default it will be 1
		 */
		cityDTO.setStateId(1L);

		/**
		 * Addition for state ends
		 */

		cityService.addCity(cityDTO);
		LOGGER.info("Outside add city ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("city.create.message", null))
				.create();
	}

	/**
	 * Update City
	 *
	 * @param accessToken
	 * @param cityDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('City','CAN_EDIT')")
	public ResponseEntity<Object> updateCity(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final CityDTO cityDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update city {}", cityDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the State in this, default it will be 1
		 */
		cityDTO.setStateId(1L);

		/**
		 * Addition for state ends
		 */
		cityService.updateCity(cityDTO);
		LOGGER.info("Outside update city ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("city.update.message", null))
				.create();
	}

	/**
	 * Get City Details based on id
	 *
	 * @param cityId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{cityId}")
	@PreAuthorize("hasPermission('City','CAN_VIEW')")
	public ResponseEntity<Object> getCity(@PathVariable("cityId") final Long cityId) throws NotFoundException {
		final CityResponseDTO cityResponseDTO = cityService.getCity(cityId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("city.detail.message", null))
				.setData(cityResponseDTO).create();
	}

	/**
	 * Get City list based on parameters
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param stateId
	 * @param searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('City','CAN_VIEW_LIST')")
	public ResponseEntity<Object> getCityListBasedOnParams(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "stateId", required = false) final Long stateId,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws ValidationException {
		Long totalCount = cityService.getCityCountBasedOnParams(activeRecords, stateId, searchKeyword);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<City> resultStates = cityService.getCityListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, activeRecords, stateId,
				searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("city.list.message", null))
				.setData(cityMapper.toDtos(resultStates)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();

	}

	/**
	 * Change status of city (active/deActive)
	 *
	 * @param accessToken
	 * @param cityId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{cityId}")
	@PreAuthorize("hasPermission('City','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("cityId") final Long cityId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of city for id {} and new status {}", cityId, active);
		cityService.changeStatus(cityId, active);
		LOGGER.info("Outside change status of city ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("city.update.message", null))
				.create();

	}

}
