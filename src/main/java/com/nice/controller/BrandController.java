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
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.BrandDTO;
import com.nice.exception.BaseException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.BrandMapper;
import com.nice.model.Brand;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.BrandService;
import com.nice.validator.BrandValidator;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@RequestMapping(path = "/brand")
@RestController
public class BrandController {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BrandController.class);
	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding brand
	 */
	@Autowired
	private BrandValidator brandValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(brandValidator);
	}

	@Autowired
	private BrandService brandService;

	@Autowired
	private BrandMapper brandMapper;

	/**
	 * Add Brand
	 *
	 * @param brandDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addBrand(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final BrandDTO brandDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Brand {}", brandDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Brand validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		brandService.addBrand(brandDTO);
		LOGGER.info("Outside add Brand ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.create.message", null))
				.create();
	}

	/**
	 * update Brand
	 *
	 * @param brandDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updateBrand(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final BrandDTO brandDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Brand {}", brandDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Brand validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		brandService.updateBrand(brandDTO);
		LOGGER.info("Outside update Brand ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.update.message", null))
				.create();
	}

	/**
	 * Get Brand Details based on id
	 *
	 * @param brandId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(name = "getBrand", value = "/{brandId}")
	public ResponseEntity<Object> getBrand(@RequestHeader("Authorization") final String accessToken, @PathVariable("brandId") final Long brandId)
			throws NotFoundException {
		LOGGER.info("Inside get Brand ");
		final BrandDTO resultBrandDTO = brandService.getBrand(brandId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.detail.message", null))
				.setData(resultBrandDTO).create();
	}

	/**
	 * Get Brand list
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(name = "getBrandList", value = "/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getBrandList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws NotFoundException {
		LOGGER.info("Inside get Brand List ");
		final Page<Brand> resultCategories = brandService.getBrandList(pageNumber, pageSize, activeRecords, searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.list.message", null))
				.setData(brandMapper.toDtos(resultCategories.getContent())).setHasNextPage(resultCategories.hasNext())
				.setHasPreviousPage(resultCategories.hasPrevious()).setTotalPages(resultCategories.getTotalPages())
				.setPageNumber(resultCategories.getNumber() + 1).setTotalCount(resultCategories.getTotalElements()).create();
	}

	/**
	 * Change status of Brand (active/deActive)
	 *
	 * @param brandId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping(name = "changeStatus", value = "/status/{brandId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("brandId") final Long brandId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of brand of id {} and status {}", brandId, active);
		brandService.changeStatus(brandId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.update.message", null))
				.create();
	}

	/**
	 * export brand list
	 *
	 * @param accessToken
	 * @param userId
	 * @param httpServletResponse
	 * @param activeRecords
	 * @return
	 * @throws IOException
	 * @throws FileOperationException
	 * @throws NotFoundException
	 */
	@Produces("text/csv")
	@GetMapping("/export/list")
	public ResponseEntity<Object> exportBrandList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse)
			throws FileOperationException {
		brandService.exportBrandList(httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.list.message", null))
				.create();
	}

	/**
	 * Upload brand
	 *
	 * @param accessToken
	 * @param userId
	 * @param file
	 * @param httpServletResponse
	 * @return
	 * @throws BaseException
	 */
	@PostMapping(path = "/upload")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = false) final MultipartFile file, final HttpServletResponse httpServletResponse) throws BaseException {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		brandService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("brand.create.message", null))
				.create();
	}
}