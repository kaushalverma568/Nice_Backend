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

import com.nice.dto.ProductVariantRequestDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductVariantService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@RequestMapping(path = "/product")
@RestController(value = "productVariantController")
public class ProductVariantController {
	/**
	 *
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ProductVariantController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	/**
	 * add/update product variant list
	 *
	 * @param accessToken
	 * @param userId
	 * @param productVariantRequestDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/{productId}/variant")
	 @PreAuthorize("hasPermission('Product List','CAN_ADD')")
	public ResponseEntity<Object> addUpdateProductVariantList(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productId") final Long productId, @RequestBody @Valid final ProductVariantRequestDTO productVariantRequestDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add ProductVariant list {}", productVariantRequestDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductVariant validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		productVariantService.addUpdateProductVariantList(productId, productVariantRequestDTO);
		LOGGER.info("Outside add ProductVariant");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.variant.create.message", null)).create();
	}

	/**
	 * Get product variant based on id(it is used by admin)
	 *
	 * @param productVariantId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping(value = "/variant/{productVariantId}")
	public ResponseEntity<Object> getProductVariant(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productVariantId") final Long productVariantId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get ProductVariant ");
		/**
		 * for admin set isAdmin to true
		 */
		final ProductVariantResponseDTO resultProductVariantDTO = productVariantService.getProductVariant(productVariantId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.variant.detail.message", null)).setData(resultProductVariantDTO).create();
	}

	/**
	 * Get product variant based on id
	 *
	 * @param productVariantId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping(value = "/variant/sku/{sku}")
	public ResponseEntity<Object> getProductVariantBySku(@RequestHeader("Authorization") final String accessToken, @PathVariable("sku") final String sku)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get ProductVariant by sku ");
		final ProductVariantResponseDTO resultProductVariantDTO = productVariantService.getProductVariantBySku(sku);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.variant.detail.message", null)).setData(resultProductVariantDTO).create();
	}

	/**
	 * Get ProductVariant list for Admin (It will give available qty 0)
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{productId}/variant/list")
	public ResponseEntity<Object> getProductVariantForProductListForAdmin(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productId") final Long productId, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get ProductVariant List ");
		/**
		 * it is for admin so setting isAdmin true
		 */
		final List<ProductVariantResponseDTO> resultProductVariantList = productVariantService.getProductVariantProductList(productId, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.variant.list.message", null)).setData(resultProductVariantList).create();
	}

	/**
	 *
	 * @param accessToken
	 * @param productId
	 * @param activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/cust/{productId}/variant/list")
	public ResponseEntity<Object> getProductVariantForProductListForCustomer(@PathVariable("productId") final Long productId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get ProductVariant List ");
		/**
		 * For customer so only active records shown and isAdmin = false
		 */
		final List<ProductVariantResponseDTO> resultProductVariantList = productVariantService.getProductVariantProductList(productId, true);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.variant.list.message", null)).setData(resultProductVariantList).create();
	}

	/**
	 * Change status of product Variant
	 *
	 * @param accessToken
	 * @param userId
	 * @param productVariantId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@PutMapping("/variant/status/{productVariantId}")
	@PreAuthorize("hasPermission('Product List','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productVariantId") final Long productVariantId, @RequestParam("active") final Boolean active)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of productVariant for id {} and new status {}", productVariantId, active);
		productVariantService.changeStatus(productVariantId, active);
		LOGGER.info("Outside change status of productVariant ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.variant.update.message", null)).create();
	}

}
