package com.nice.controller;

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

import com.nice.dto.CategoryDTO;
import com.nice.dto.CategoryResponseDTO;
import com.nice.exception.BaseException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CategoryMapper;
import com.nice.model.Category;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CategoryService;
import com.nice.validator.CategoryValidator;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@RequestMapping(path = "/category")
@RestController
public class CategoryController {
	/**
	 *
	 */
	private static final String LIST_MESSAGE = "category.list.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CategoryController.class);
	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding category
	 */
	@Autowired
	private CategoryValidator categoryValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(categoryValidator);
	}

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private CategoryMapper categoryMapper;

	/**
	 * Add Category
	 *
	 * @param categoryDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Category','CAN_ADD')")
	public ResponseEntity<Object> addCategory(@RequestHeader("Authorization") final String accessToken, @ModelAttribute @Valid final CategoryDTO categoryDTO,
			final BindingResult result, @RequestParam(name = "image", required = false) final MultipartFile image)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Category {}", categoryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Category validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		categoryService.addCategory(categoryDTO, image);
		LOGGER.info("Outside add Category ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("category.create.message", null))
				.create();
	}

	/**
	 * update Category
	 *
	 * @param categoryDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Category','CAN_EDIT')")
	public ResponseEntity<Object> updateCategory(@RequestHeader("Authorization") final String accessToken, @ModelAttribute @Valid final CategoryDTO categoryDTO,
			final BindingResult result, @RequestParam(name = "image", required = false) final MultipartFile image)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Category {}", categoryDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Category validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		categoryService.updateCategory(categoryDTO, image);
		LOGGER.info("Outside update Category ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("category.update.message", null))
				.create();
	}

	/**
	 * Get Category Details based on id
	 *
	 * @param categoryId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{categoryId}")
	@PreAuthorize("hasPermission('Category','CAN_VIEW')")
	public ResponseEntity<Object> getCategory(@RequestHeader("Authorization") final String accessToken, @PathVariable("categoryId") final Long categoryId)
			throws NotFoundException {
		LOGGER.info("Inside get Category ");
		final CategoryResponseDTO resultCategoryDTO = categoryService.getCategory(categoryId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("category.detail.message", null))
				.setData(resultCategoryDTO).create();
	}

	/**
	 * Get Category list
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Category','CAN_VIEW_LIST')")
	public ResponseEntity<Object> getCategoryList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword,
			@RequestParam(name = "vendorId", required = false) final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside get Category List ");
		final Page<Category> resultCategories = categoryService.getCategoryList(pageNumber, pageSize, activeRecords, searchKeyword, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(LIST_MESSAGE, null))
				.setData(categoryMapper.toDtos(resultCategories.getContent())).setHasNextPage(resultCategories.hasNext())
				.setHasPreviousPage(resultCategories.hasPrevious()).setTotalPages(resultCategories.getTotalPages())
				.setPageNumber(resultCategories.getNumber() + 1).setTotalCount(resultCategories.getTotalElements()).create();
	}

	/**
	 * Change status of Category (active/deActive)
	 *
	 * @param categoryId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{categoryId}")
	@PreAuthorize("hasPermission('Category','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("categoryId") final Long categoryId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of category of id {} and status {}", categoryId, active);
		categoryService.changeStatus(categoryId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("category.update.message", null))
				.create();
	}

	/**
	 * export category list
	 *
	 * @param accessToken
	 * @param userId
	 * @param httpServletResponse
	 * @param activeRecords
	 * @return
	 * @throws FileOperationException
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@Produces("text/csv")
	@GetMapping("/export/list")
	@PreAuthorize("hasPermission('Category','CAN_EXPORT')")
	public ResponseEntity<Object> exportCategoryList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse)
			throws FileOperationException, ValidationException, NotFoundException {
		categoryService.exportCategoryList(httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(LIST_MESSAGE, null)).create();
	}

	/**
	 * Upload category
	 *
	 * @param accessToken
	 * @param userId
	 * @param file
	 * @param httpServletResponse
	 * @return
	 * @throws BaseException
	 */
	@PostMapping(path = "/upload")
	@PreAuthorize("hasPermission('Category','CAN_IMPORT')")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = false) final MultipartFile file, final HttpServletResponse httpServletResponse) throws BaseException {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		categoryService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("category.create.message", null))
				.create();
	}

	/**
	 * to delete image by type
	 *
	 * @param accessToken
	 * @param categoryId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@DeleteMapping("/image/{categoryId}")
	public ResponseEntity<Object> deleteImage(@RequestHeader("Authorization") final String accessToken, @PathVariable("categoryId") final Long categoryId)
			throws NotFoundException {
		categoryService.deleteImage(categoryId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("category.update.message", null))
				.create();
	}
}