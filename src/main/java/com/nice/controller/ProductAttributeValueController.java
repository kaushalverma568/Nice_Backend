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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.ProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductAttributeValueService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 02-Jul-2020
 */
@RequestMapping(path = "/product/attribute/value")
@RestController
public class ProductAttributeValueController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductAttributeValueController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@PostMapping("/{productVariantId}")
	@PreAuthorize("hasPermission('Product Attribute','CAN_ADD')")
	public ResponseEntity<Object> addUpdateProductAttributeValue(@PathVariable final Long productVariantId,
			@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final List<ProductAttributeValueDTO> productAttributeValueDTOList,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add ProductAttributeValue {}", productAttributeValueDTOList);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductAttributeValue validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		productAttributeValueService.addUpdateProductAttributeValue(productAttributeValueDTOList, productVariantId);
		LOGGER.info("Outside add ProductAttributeValue {}", productAttributeValueDTOList);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.value.create.message", null)).create();
	}

	@GetMapping(value = "/{productAttributeValueId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productAttributeValueId") final Long productAttributeValueId) throws NotFoundException {
		LOGGER.info("Inside getById ProductAttributeValue {}", productAttributeValueId);
		ProductAttributeValueDTO resultProductAttributeValue = productAttributeValueService.getProductAttributeValue(productAttributeValueId);
		LOGGER.info("After getById ProductAttributeValue {}", productAttributeValueId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.value.detail.message", null)).setData(resultProductAttributeValue).create();
	}

	@GetMapping("/list/{productVariantId}")
	public ResponseEntity<Object> getList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long productVariantId,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) throws NotFoundException, ValidationException {
		LOGGER.info("Inside getList ProductAttributeValue for ProductVariant : {} and active :{}", productVariantId, activeRecords);
		final List<ProductAttributeValueDTO> productAttributeValueDtoList = productAttributeValueService.getDtoListWithUserCheck(activeRecords,
				productVariantId);
		LOGGER.info("After getList ProductAttributeValue for ProductVariant : {} and active :{}", productVariantId, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.value.list.message", null)).setData(productAttributeValueDtoList).create();
	}

	@GetMapping("/cust/list/{productVariantId}")
	public ResponseEntity<Object> getList(@PathVariable final Long productVariantId) throws NotFoundException {
		LOGGER.info("Inside getList For customer ProductAttributeValue for ProductVariant : {} ", productVariantId);
		final List<ProductAttributeValueDTO> productAttributeValueDtoList = productAttributeValueService.getList(productVariantId, true);
		LOGGER.info("After getList For customer ProductAttributeValue for ProductVariant : {} ", productVariantId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.value.list.message", null)).setData(productAttributeValueDtoList).create();
	}

	@PutMapping("/status/{productAttributeValueId}")
	@PreAuthorize("hasPermission('Product Attribute','CAN_DELETE')")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productAttributeValueId") final Long productAttributeValueId, @RequestParam final Boolean active)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of ProductAttributeValue of id {} and status {}", productAttributeValueId, active);
		productAttributeValueService.changeStatus(productAttributeValueId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.attribute.value.update.message", null)).create();
	}
}