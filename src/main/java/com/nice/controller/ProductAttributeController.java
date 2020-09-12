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
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ProductAttributeDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAttributeMapper;
import com.nice.model.ProductAttribute;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductAttributeService;
import com.nice.validator.ProductAttributeValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 02-Jul-2020
 */
@RequestMapping(path = "/product/attribute")
@RestController
public class ProductAttributeController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductAttributeController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductAttributeService productAttributeService;

	@Autowired
	private ProductAttributeMapper productAttributeMapper;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private ProductAttributeValidator productAttributeValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(productAttributeValidator);
	}

	/**
	 * Add product attribute
	 *
	 * @param  accessToken
	 * @param  productAttributeDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Product Attribute','CAN_ADD')")
	public ResponseEntity<Object> addProductAttribute(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final ProductAttributeDTO productAttributeDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add ProductAttribute {}", productAttributeDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductAttribute validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		ProductAttributeDTO resultProductAttribute = productAttributeService.addProductAttribute(productAttributeDTO);
		LOGGER.info("Outside add ProductAttribute {}", resultProductAttribute);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.create.message", null)).setData(resultProductAttribute).create();
	}

	/**
	 * Update product attribute
	 *
	 * @param  accessToken
	 * @param  productAttributeDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Product Attribute','CAN_EDIT')")
	public ResponseEntity<Object> updateProductAttribute(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final ProductAttributeDTO productAttributeDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update ProductAttribute {}", productAttributeDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductAttribute validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		ProductAttributeDTO resultProductAttribute = productAttributeService.updateProductAttribute(productAttributeDTO);
		LOGGER.info("Outside update ProductAttribute {}", resultProductAttribute);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.update.message", null)).setData(resultProductAttribute).create();
	}

	/**
	 * Get product attribute
	 *
	 * @param  accessToken
	 * @param  productAttributeId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping(value = "/{productAttributeId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productAttributeId") final Long productAttributeId) throws NotFoundException, ValidationException {
		ProductAttributeDTO resultProductAttribute = productAttributeService.getProductAttribute(productAttributeId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.detail.message", null)).setData(resultProductAttribute).create();
	}

	/**
	 * Get product attribute list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  vendorId
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "vendorId", required = true) final Long vendorId) throws ValidationException {
		final Page<ProductAttribute> resultProductAttribute = productAttributeService.getList(pageNumber, pageSize, activeRecords, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.list.message", null))
				.setData(productAttributeMapper.toDtos(resultProductAttribute.getContent())).setHasNextPage(resultProductAttribute.hasNext())
				.setHasPreviousPage(resultProductAttribute.hasPrevious()).setTotalPages(resultProductAttribute.getTotalPages())
				.setPageNumber(resultProductAttribute.getNumber() + 1).setTotalCount(resultProductAttribute.getTotalElements()).create();
	}

	/**
	 * get all product attribute without pagination
	 *
	 * @param  accessToken
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/active/all")
	public ResponseEntity<Object> getAllListWithoutPagination(@RequestHeader("Authorization") final String accessToken) throws ValidationException {
		final List<ProductAttribute> resultProductAttribute = productAttributeService.getAllActiveList();
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.list.message", null))
				.setData(productAttributeMapper.toDtos(resultProductAttribute)).create();
	}

	/**
	 * change status product attribute(active/deactive)
	 *
	 * @param  accessToken
	 * @param  productAttributeId
	 * @param  active
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/status/{productAttributeId}")
	@PreAuthorize("hasPermission('Product Attribute','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productAttributeId") final Long productAttributeId, @RequestParam final Boolean active)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of ProductAttribute of id {} and status {}", productAttributeId, active);
		productAttributeService.changeStatus(productAttributeId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.update.message", null)).create();
	}

	/**
	 * import product attribute
	 *
	 * @param  accessToken
	 * @param  file
	 * @param  httpServletResponse
	 * @return
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	@PostMapping(path = "/upload")
	@PreAuthorize("hasPermission('Product Attribute','CAN_ADD')")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = true) final MultipartFile file, final HttpServletResponse httpServletResponse)
			throws ValidationException, FileOperationException {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		productAttributeService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.create.message", null)).create();
	}
}