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

import com.nice.dto.AreaDTO;
import com.nice.dto.AreaResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.AreaMapper;
import com.nice.model.Area;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.AreaService;
import com.nice.validator.AreaValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@RequestMapping(path = "/area")
@RestController
public class AreaController {

	private static final Logger LOGGER = LoggerFactory.getLogger(AreaController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding area
	 */
	@Autowired
	private AreaValidator areaValidator;

	@Autowired
	private AreaMapper areaMapper;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(areaValidator);
	}

	@Autowired
	private AreaService areaService;

	/**
	 * Add Area
	 *
	 * @param  accessToken
	 * @param  areaDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Area','CAN_ADD')")
	public ResponseEntity<Object> addArea(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final AreaDTO areaDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add area {}", areaDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Area validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the city in this, default it will be 1
		 */
		areaDTO.setCityId(1L);

		areaService.addArea(areaDTO);
		LOGGER.info("Outside add area");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("area.create.message", null))
				.create();
	}

	/**
	 * Update Area
	 *
	 * @param  accessToken
	 * @param  areaDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Area','CAN_EDIT')")
	public ResponseEntity<Object> updateArea(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final AreaDTO areaDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update area {}", areaDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Area validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the city in this, default it will be 1
		 */
		areaDTO.setCityId(1L);

		areaService.updateArea(areaDTO);
		LOGGER.info("Outside update area");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("area.update.message", null))
				.create();
	}

	/**
	 * Get Area Details based on id
	 *
	 * @param  areaId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{areaId}")
	public ResponseEntity<Object> getArea(@PathVariable("areaId") final Long areaId) throws NotFoundException {
		final AreaResponseDTO resultArea = areaService.getArea(areaId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("area.detail.message", null))
				.setData(resultArea).create();
	}

	/**
	 * Get Area list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getAreaList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword,
			@RequestParam(name = "cityId", required = false) final Long cityId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get area list searchKeyword {} and activeRecords {}", searchKeyword, activeRecords);
		final Page<Area> resultAreas = areaService.getAreaList(pageNumber, pageSize, activeRecords, searchKeyword, cityId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("area.list.message", null))
				.setData(areaMapper.toDtos(resultAreas.getContent())).setHasNextPage(resultAreas.hasNext()).setHasPreviousPage(resultAreas.hasPrevious())
				.setTotalPages(resultAreas.getTotalPages()).setPageNumber(resultAreas.getNumber() + 1).setTotalCount(resultAreas.getTotalElements()).create();
	}

	/**
	 * Change status of area (active/deActive)
	 *
	 * @param  accessToken
	 * @param  areaId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{areaId}")
	@PreAuthorize("hasPermission('Area','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("areaId") final Long areaId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of area id {} and status {}", areaId, active);
		areaService.changeStatus(areaId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("area.update.message", null))
				.create();
	}
}
