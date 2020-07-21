package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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

import com.nice.dto.ProductToppingDto;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductToppingService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@RequestMapping(path = "/product/topping")
@RestController
public class ProductToppingController {

	/**
	 *
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ProductToppingController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductToppingService productToppingService;

	/**
	 * @param accessToken
	 * @param image
	 * @param userId
	 * @param productToppingDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/{productVariantId}")
	public ResponseEntity<Object> addProductTopping(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long productVariantId,
			@Valid @RequestBody final List<ProductToppingDto> productToppingDTOList, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Product Topping {}", productToppingDTOList);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Product validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		productToppingService.addUpdateProductTopping(productToppingDTOList, productVariantId);
		LOGGER.info("Outside add Product Topping");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.create.message", null))
				.create();
	}

	/**
	 *
	 * @param productToppingId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{productToppingId}")
	public ResponseEntity<Object> getProductTopping(@PathVariable("productToppingId") final Long productToppingId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside getProductTopping ToppingId {}", productToppingId);
		final ProductToppingResponseDTO productToppingDTO = productToppingService.getProductTopping(productToppingId);
		LOGGER.info("After getProductTopping ToppingId {}", productToppingId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.detail.message", null))
				.setData(productToppingDTO).create();
	}

	/**
	 *
	 * @param productVariantId
	 * @param active
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/list/{productVariantId}")
	public ResponseEntity<Object> getProductListBasedOnParams(@PathVariable final Long productVariantId,
			@RequestParam(required = false) final Boolean activeRecords) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Topping List for Product Variant {}", productVariantId);
		List<ProductToppingResponseDTO> productToppingList = productToppingService.getDtoListWithUserCheck(activeRecords, productVariantId);
		LOGGER.info("After get Topping List for Product Variant {}", productVariantId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.list.message", null))
				.setData(productToppingList).create();
	}

	/**
	 *
	 * @param productVariantId
	 * @param activeRecords
	 * @return
	 */
	@GetMapping("/cust/list/{productVariantId}")
	public ResponseEntity<Object> getProductList(@PathVariable final Long productVariantId, @RequestParam(required = false) final Boolean activeRecords) {
		LOGGER.info("Inside get Topping List for Product varinat {}", productVariantId);
		List<ProductToppingResponseDTO> productToppingList = productToppingService.getToppingForProductVariant(productVariantId, true);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.list.message", null))
				.setData(productToppingList).create();
	}

	/**
	 * Change status of product
	 *
	 * @param accessToken
	 * @param userId
	 * @param productId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@PutMapping("/status/{productToppingId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("productToppingId") final Long productToppingId, @RequestParam("active") final Boolean active)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of Topping for id {} and new status {}", productToppingId, active);
		productToppingService.changeStatus(productToppingId, active);
		LOGGER.info("Outside change status of product ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.update.message", null))
				.create();
	}
}
