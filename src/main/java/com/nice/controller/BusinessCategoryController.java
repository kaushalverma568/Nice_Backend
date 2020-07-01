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
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
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

import com.nice.dto.BusinessCategoryDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.BusinessCategoryMapper;
import com.nice.model.BusinessCategory;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.BusinessCategoryService;
import com.nice.util.CommonUtility;
import com.nice.validator.BusinessCategoryValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@RequestMapping(path = "/business/category")
@RestController
public class BusinessCategoryController {

	private static final Logger LOGGER = LoggerFactory.getLogger(BusinessCategoryController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private BusinessCategoryService businessCategoryService;

	@Autowired
	private BusinessCategoryMapper businessCategoryMapper;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private BusinessCategoryValidator businessCategoryValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(businessCategoryValidator);
	}


	@PostMapping
	public ResponseEntity<Object> addBusinessCategory(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "image", required = false) final MultipartFile image,
			@ModelAttribute @Valid final BusinessCategoryDTO businessCategoryDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add BusinessCategory {}", businessCategoryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("BusinessCategory validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		if (image == null || !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(image.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		BusinessCategoryDTO resultBusinessCategory = businessCategoryService.addBusinessCategory(businessCategoryDTO, image);
		LOGGER.info("Outside add BusinessCategory {}", resultBusinessCategory);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("business.category.create.message", null)).setData(resultBusinessCategory).create();
	}

	@PutMapping
	public ResponseEntity<Object> updateBusinessCategory(@RequestHeader("Authorization") final String accessToken,	
			@RequestParam(name = "image", required = false) final MultipartFile image,
			@ModelAttribute @Valid final BusinessCategoryDTO businessCategoryDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update BusinessCategory {}", businessCategoryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("BusinessCategory validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		BusinessCategoryDTO resultBusinessCategory = businessCategoryService.updateBusinessCategory(businessCategoryDTO, image);
		LOGGER.info("Outside update BusinessCategory {}", resultBusinessCategory);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("business.category.update.message", null)).setData(resultBusinessCategory).create();
	}

	@GetMapping(value = "/{businessCategoryId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken, @PathVariable("businessCategoryId") final Long businessCategoryId)
			throws NotFoundException {
		BusinessCategoryDTO resultBusinessCategory = businessCategoryService.getBusinessCategory(businessCategoryId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("business.category.detail.message", null)).setData(resultBusinessCategory).create();
	}

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) {
		final Page<BusinessCategory> resultBusinessCategory = businessCategoryService.getList(pageNumber, pageSize, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("business.category.list.message", null)).setData(businessCategoryMapper.toDtos(resultBusinessCategory.getContent()))
				.setHasNextPage(resultBusinessCategory.hasNext()).setHasPreviousPage(resultBusinessCategory.hasPrevious()).setTotalPages(resultBusinessCategory.getTotalPages())
				.setPageNumber(resultBusinessCategory.getNumber() + 1).setTotalCount(resultBusinessCategory.getTotalElements()).create();
	}

	@PutMapping("/status/{businessCategoryId}")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("businessCategoryId") final Long businessCategoryId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of BusinessCategory of id {} and status {}", businessCategoryId, active);
		businessCategoryService.changeStatus(businessCategoryId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("business.category.update.message", null )).create();
	}
}