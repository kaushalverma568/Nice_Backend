package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMasterMapper;
import com.nice.model.ProductExtrasMaster;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductExtrasMasterService;
import com.nice.validator.ProductExtrasMasterValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@RequestMapping(path = "/product/extras/master")
@RestController
public class ProductExtrasMasterController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductExtrasMasterController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductExtrasMasterService productExtrasMasterService;
	
	@Autowired
	private ProductExtrasMasterMapper productExtrasMasterMapper;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private ProductExtrasMasterValidator productExtrasMasterValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(productExtrasMasterValidator);
	}

	@PostMapping
	public ResponseEntity<Object> addProductExtrasMaster(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final ProductExtrasMasterDTO productExtrasMasterDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add ProductExtrasMaster {}", productExtrasMasterDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductExtrasMaster validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long productExtrasMasterId = productExtrasMasterService.addProductExtrasMaster(productExtrasMasterDTO);
		LOGGER.info("Outside add ProductExtrasMaster {}", productExtrasMasterId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.master.create.message", null)).setData(productExtrasMasterId).create();
	}

	@PutMapping
	public ResponseEntity<Object> updateProductExtrasMaster(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final ProductExtrasMasterDTO productExtrasMasterDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update ProductExtrasMaster {}", productExtrasMasterDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductExtrasMaster validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long productExtrasMasterId = productExtrasMasterService.updateProductExtrasMaster(productExtrasMasterDTO);
		LOGGER.info("Outside update ProductExtrasMaster {}", productExtrasMasterId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.master.update.message", null)).setData(productExtrasMasterId).create();
	}

	@GetMapping(value = "/{productExtrasMasterId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken, @PathVariable("productExtrasMasterId") final Long productExtrasMasterId)
			throws NotFoundException {
		ProductExtrasMasterDTO resultProductExtrasMaster = productExtrasMasterService.getProductExtrasMaster(productExtrasMasterId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.master.detail.message", null)).setData(resultProductExtrasMaster).create();
	}


	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getToppingList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "vendorId", required = false) final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside get Topping List ");
		final Page<ProductExtrasMaster> resultExtrasMasterList = productExtrasMasterService.getList(pageNumber, pageSize, activeRecords, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.extras.master.update.message", null))
				.setData(productExtrasMasterMapper.toDtos(resultExtrasMasterList.getContent())).setHasNextPage(resultExtrasMasterList.hasNext())
				.setHasPreviousPage(resultExtrasMasterList.hasPrevious()).setTotalPages(resultExtrasMasterList.getTotalPages()).setPageNumber(resultExtrasMasterList.getNumber() + 1)
				.setTotalCount(resultExtrasMasterList.getTotalElements()).create();
	}

	@PutMapping("/status/{productExtrasMasterId}")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken, 
			@PathVariable("productExtrasMasterId") final Long productExtrasMasterId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of ProductExtrasMaster of id {} and status {}", productExtrasMasterId, active);
		productExtrasMasterService.changeStatus(productExtrasMasterId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.master.update.message", null)).create();
	}
	
	/**
	 * 
	 * @param accessToken
	 * @param file
	 * @param httpServletResponse
	 * @return
	 * @throws ValidationException 
	 * @throws FileOperationException 
	 */
	@PostMapping(path = "/upload")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = true) final MultipartFile file, final HttpServletResponse httpServletResponse) throws ValidationException, FileOperationException  {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		productExtrasMasterService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.extras.master.create.message", null))
				.create();
	}
}