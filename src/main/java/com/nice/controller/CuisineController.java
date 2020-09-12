/**
 *
 */
package com.nice.controller;

import java.io.IOException;
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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CuisineDTO;
import com.nice.dto.CuisineResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CuisineMapper;
import com.nice.model.Cuisine;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CuisineService;
import com.nice.validator.CuisineValidator;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@RequestMapping(path = "/cuisine")
@RestController
public class CuisineController {

	/**
	 *
	 */
	private static final String CUISINE_UPDATE_MESSAGE = "cuisine.update.message";
	private static final Logger LOGGER = LoggerFactory.getLogger(CuisineController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CuisineService cuisineService;

	@Autowired
	private CuisineMapper cuisineMapper;

	/**
	 * Validator - to apply/check any type of validation regarding category
	 */
	@Autowired
	private CuisineValidator cuisineValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(cuisineValidator);
	}

	/**
	 * add cuisine
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  image
	 * @param  cuisineDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws IOException
	 */

	@PostMapping
	@PreAuthorize("hasPermission('Cuisine','CAN_ADD')")
	public ResponseEntity<Object> addCuisine(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "image", required = false) final MultipartFile image, @ModelAttribute @Valid final CuisineDTO cuisineDTO,
			final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add cuisine method {}", cuisineDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("cuisine validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		cuisineService.addCuisine(cuisineDTO, image);
		LOGGER.info("Outside add cuisine method ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cuisine.create.message", null))
				.create();
	}

	/**
	 * update cuisine
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  cuisineDTO
	 * @param  result
	 * @param  image
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	@PutMapping
	@PreAuthorize("hasPermission('Cuisine','CAN_EDIT')")
	public ResponseEntity<Object> updateCuisine(@RequestHeader("Authorization") final String accessToken, @ModelAttribute @Valid final CuisineDTO cuisineDTO,
			final BindingResult result, @RequestParam(name = "image", required = false) final MultipartFile image)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update cuisine method {}", cuisineDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("cuisine validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		cuisineService.updateCuisine(cuisineDTO, image);
		LOGGER.info("Outside update cuisine method {}", cuisineDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(CUISINE_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * get cuisine
	 *
	 * @param  cuisineId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{cuisineId}")
	public ResponseEntity<Object> getCuisine(@PathVariable("cuisineId") final Long cuisineId) throws NotFoundException {
		LOGGER.info("Inside get cuisine method {}", cuisineId);
		final CuisineResponseDTO resultCuisine = cuisineService.getCuisine(cuisineId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cuisine.detail.message", null))
				.setData(resultCuisine).create();
	}

	/**
	 * To get list of cuisines
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getCuisineList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get cuisine List ");
		final Page<Cuisine> resultCuisines = cuisineService.getCuisineList(pageNumber, pageSize, activeRecords, searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cuisine.list.message", null))
				.setData(cuisineMapper.toDtos(resultCuisines.getContent())).setHasNextPage(resultCuisines.hasNext())
				.setHasPreviousPage(resultCuisines.hasPrevious()).setTotalPages(resultCuisines.getTotalPages()).setPageNumber(resultCuisines.getNumber() + 1)
				.setTotalCount(resultCuisines.getTotalElements()).create();
	}

	/**
	 * Change status of cuisine (active/deActive)
	 *
	 * @param  cuisineId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping(name = "changeStatus", value = "/status/{cuisineId}")
	@PreAuthorize("hasPermission('Cuisine','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("cuisineId") final Long cuisineId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of cuisine of id {} and status {}", cuisineId, active);
		cuisineService.changeStatus(cuisineId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(CUISINE_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Delete Image
	 *
	 * @param  cuisineId
	 * @return
	 * @throws NotFoundException
	 */
	@DeleteMapping("/image/{cuisineId}")
	@PreAuthorize("hasPermission('Cuisine','CAN_EDIT')")
	public ResponseEntity<Object> deleteImage(@RequestHeader("Authorization") final String accessToken, @PathVariable("cuisineId") final Long cuisineId)
			throws NotFoundException {
		LOGGER.info("Inside delete image of cuisine of id {}", cuisineId);
		cuisineService.deleteImage(cuisineId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(CUISINE_UPDATE_MESSAGE, null))
				.create();
	}
}
