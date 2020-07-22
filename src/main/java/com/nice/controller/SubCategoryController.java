package com.nice.controller;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.ws.rs.Produces;

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
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.SubCategoryDTO;
import com.nice.dto.SubCategoryResponseDTO;
import com.nice.exception.BaseException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SubCategoryMapper;
import com.nice.model.SubCategory;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.SubCategoryService;
import com.nice.validator.SubCategoryValidator;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@RequestMapping(path = "/subcategory")
@RestController
public class SubCategoryController {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SubCategoryController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding subCategory
	 */
	@Autowired
	private SubCategoryValidator subCategoryValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(subCategoryValidator);
	}

	@Autowired
	private SubCategoryService subCategoryService;

	@Autowired
	private SubCategoryMapper subCategoryMapper;

	/**
	 * Add sub category
	 *
	 * @param  subCategoryDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Sub Category','CAN_ADD')")
	public ResponseEntity<Object> addSubCategory(@RequestHeader("Authorization") final String accessToken,
			@ModelAttribute @Valid final SubCategoryDTO subCategoryDTO, final BindingResult result,
			@RequestParam(name = "image", required = false) final MultipartFile image) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add sub category {}", subCategoryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("sub category validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		subCategoryService.addSubCategory(subCategoryDTO, image);
		LOGGER.info("Outside add sub category ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.create.message", null))
				.create();

	}

	/**
	 * Update sub category
	 *
	 * @param  subCategoryDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Sub Category','CAN_EDIT')")
	public ResponseEntity<Object> updateSubCategory(@RequestHeader("Authorization") final String accessToken,
			@ModelAttribute @Valid final SubCategoryDTO subCategoryDTO, final BindingResult result,
			@RequestParam(name = "image", required = false) final MultipartFile image) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update sub category {}", subCategoryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("sub category validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		subCategoryService.updateSubCategory(subCategoryDTO, image);
		LOGGER.info("Outside update sub category ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.update.message", null))
				.create();
	}

	/**
	 * Get sub category
	 *
	 * @param  subCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{subCategoryId}")
	@PreAuthorize("hasPermission('Sub Category','CAN_VIEW')")
	public ResponseEntity<Object> getSubCategory(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("subCategoryId") final Long subCategoryId) throws NotFoundException {
		LOGGER.info("Inside get sub category for id:{}", subCategoryId);
		final SubCategoryResponseDTO resultSubCategoryResponseDTO = subCategoryService.getSubCategory(subCategoryId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.detail.message", null))
				.setData(resultSubCategoryResponseDTO).create();
	}

	/**
	 * Get sub category List
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Sub Category','CAN_VIEW_LIST')")
	public ResponseEntity<Object> getSubCategoryList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "categoryId", required = false) final Long categoryId) throws NotFoundException {
		LOGGER.info("Inside get sub category List");
		final Page<SubCategory> resultSubCategories = subCategoryService.getSubCategoryList(pageNumber, pageSize, activeRecords, categoryId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.list.message", null))
				.setData(subCategoryMapper.toDtos(resultSubCategories.getContent())).setHasNextPage(resultSubCategories.hasNext())
				.setHasPreviousPage(resultSubCategories.hasPrevious()).setTotalPages(resultSubCategories.getTotalPages())
				.setPageNumber(resultSubCategories.getNumber() + 1).setTotalCount(resultSubCategories.getTotalElements()).create();
	}

	/**
	 * Change Status of sub category (Active/DeActive)
	 *
	 * @param  subCategoryId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{subCategoryId}")
	@PreAuthorize("hasPermission('Sub Category','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("subCategoryId") final Long subCategoryId, @RequestParam("active") final Boolean active)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of SubCategory of id {} and status {}", subCategoryId, active);
		subCategoryService.changeStatus(subCategoryId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.update.message", null))
				.create();
	}

	/**
	 * export sub category list
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  httpServletResponse
	 * @param  activeRecords
	 * @return
	 * @throws FileOperationException
	 * @throws ValidationException
	 * @throws IOException
	 * @throws NotFoundException
	 */
	@Produces("text/csv")
	@GetMapping("/export/list")
	@PreAuthorize("hasPermission('Sub Category','CAN_EXPORT')")
	public ResponseEntity<Object> exportSubCategoryList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse)
			throws FileOperationException, ValidationException, NotFoundException {
		subCategoryService.exportSubCategoryList(httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.list.message", null))
				.create();
	}

	/**
	 * Upload sub category
	 *
	 * @param  accessToken
	 * @param  file
	 * @param  httpServletResponse
	 * @return
	 * @throws BaseException
	 */
	@PostMapping(path = "/upload")
	@PreAuthorize("hasPermission('Sub Category','CAN_IMPORT')")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = false) final MultipartFile file, final HttpServletResponse httpServletResponse) throws BaseException {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		subCategoryService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("subcategory.create.message", null))
				.create();
	}
}
