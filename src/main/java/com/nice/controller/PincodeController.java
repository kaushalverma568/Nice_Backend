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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.PaginationUtilDto;
import com.nice.dto.PincodeDTO;
import com.nice.dto.PincodeResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PincodeMapper;
import com.nice.model.Pincode;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.PincodeService;
import com.nice.util.PaginationUtil;
import com.nice.validator.PincodeValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@RequestMapping(path = "/pincode")
@RestController
public class PincodeController {

	private static final Logger LOGGER = LoggerFactory.getLogger(PincodeController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding pincode
	 */
	@Autowired
	private PincodeValidator pincodeValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(pincodeValidator);
	}

	@Autowired
	private PincodeService pincodeService;

	@Autowired
	private PincodeMapper pincodeMapper;

	/**
	 * Add Pincode
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  pincodeDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addPincode(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final PincodeDTO pincodeDTO,
			final BindingResult bindingResult) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add pincode {}", pincodeDTO);
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		pincodeService.addPincode(pincodeDTO);
		LOGGER.info("Outside add pincode ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("pincode.create.message", null))
				.create();
	}

	/**
	 * Get Pincode Details based on id
	 *
	 * @param  pincodeId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{pincodeId}")
	public ResponseEntity<Object> getPincode(@PathVariable("pincodeId") final Long pincodeId) throws NotFoundException {
		final PincodeResponseDTO pincodeResponseDTO = pincodeService.getPincode(pincodeId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("pincode.detail.message", null))
				.setData(pincodeResponseDTO).create();
	}

	/**
	 * Get Pincode list based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  cityId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getPincodeListBasedOnParams(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "cityId", required = false) final Long cityId,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws ValidationException {
		Long totalCount = pincodeService.getPincodeCountBasedOnParams(activeRecords, cityId, searchKeyword);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<Pincode> resultPincodes = pincodeService.getPincodeListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, activeRecords, cityId,
				searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("pincode.list.message", null))
				.setData(pincodeMapper.toDtos(resultPincodes)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();

	}

	/**
	 * Change status of pin code (active/deActive)
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  pincodeId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{pincodeId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("pincodeId") final Long pincodeId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of pincode for id {} and new status {}", pincodeId, active);
		pincodeService.changeStatus(pincodeId, active);
		LOGGER.info("Outside change status of pincode ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("pincode.update.message", null))
				.create();
	}

}
