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

import com.nice.dto.PaginationUtilDto;
import com.nice.dto.StateDTO;
import com.nice.dto.StateResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.StateMapper;
import com.nice.model.State;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.StateService;
import com.nice.util.PaginationUtil;
import com.nice.validator.StateValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@RequestMapping(path = "/state")
@RestController
public class StateController {

	private static final Logger LOGGER = LoggerFactory.getLogger(StateController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding state
	 */
	@Autowired
	private StateValidator stateValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(stateValidator);
	}

	@Autowired
	private StateService stateService;

	@Autowired
	private StateMapper stateMapper;

	/**
	 * Add State
	 *
	 * @param  accessToken
	 * @param  stateDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('State','CAN_ADD')")
	public ResponseEntity<Object> addState(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final StateDTO stateDTO,
			final BindingResult bindingResult) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add state {}", stateDTO);
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}

		/**
		 * Added to handle the country in this, default it will be 1
		 */
		stateDTO.setCountryId(1L);

		/**
		 * Addition for country ends
		 */

		State resultState = stateService.addState(stateDTO);
		LOGGER.info("Outside add state {}", resultState);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("state.create.message", null))
				.setData(resultState).create();
	}

	/**
	 * Update State
	 *
	 * @param  accessToken
	 * @param  stateDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('State','CAN_EDIT')")
	public ResponseEntity<Object> updateState(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final StateDTO stateDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update state {}", stateDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}

		/**
		 * Added to handle the country in this, default it will be 1
		 */
		stateDTO.setCountryId(1L);

		/**
		 * Addition for country ends
		 */
		final State resultState = stateService.updateState(stateDTO);
		LOGGER.info("Outside update state {}", resultState);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("state.update.message", null))
				.setData(resultState).create();
	}

	/**
	 * Get State Details based on id
	 *
	 * @param  stateId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{stateId}")
	public ResponseEntity<Object> getState(@PathVariable("stateId") final Long stateId) throws NotFoundException {
		final StateResponseDTO stateResponseDTO = stateService.getState(stateId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("state.detail.message", null))
				.setData(stateResponseDTO).create();
	}

	/**
	 * Get state list based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  countryId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getStateListBasedOnParams(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "countryId", required = false) final Long countryId,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws ValidationException {
		Long totalCount = stateService.getStateCountBasedOnParams(activeRecords, countryId, searchKeyword);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<State> resultStates = stateService.getStateListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, activeRecords, countryId,
				searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("state.list.message", null))
				.setData(stateMapper.toDtos(resultStates)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();

	}

	/**
	 * Change status of state (active/deActive)
	 *
	 * @param  accessToken
	 * @param  stateId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{stateId}")
	@PreAuthorize("hasPermission('State','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("stateId") final Long stateId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of state for id {} and status {}", stateId, active);
		stateService.changeStatus(stateId, active);
		LOGGER.info("Outside change status of state for id {} ", stateId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("state.update.message", null))
				.create();

	}

}
