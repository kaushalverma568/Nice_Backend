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

import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductAddonsService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@RequestMapping(path = "/product/addons")
@RestController
public class ProductAddonsController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductAddonsController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductAddonsService productAddonsService;

	@PostMapping("/{productVariantId}")
	public ResponseEntity<Object> addProductAddons(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long productVariantId,
			@RequestBody @Valid final List<ProductAddonsDTO> productAddonsDtoList, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add ProductAddons {}", productAddonsDtoList);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductAddons validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		productAddonsService.addUpdateProductAddons(productAddonsDtoList, productVariantId);
		LOGGER.info("Outside add ProductAddons ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.create.message", null))
				.create();
	}

	@GetMapping(value = "/{productAddonsId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken, @PathVariable("productAddonsId") final Long productAddonsId)
			throws NotFoundException {
		ProductAddonsDTO resultProductAddons = productAddonsService.getProductAddons(productAddonsId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.detail.message", null))
				.setData(resultProductAddons).create();
	}

	@GetMapping("/list/{productVariantId}")
	public ResponseEntity<Object> getList(@PathVariable final Long productVariantId,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) throws NotFoundException {
		final List<ProductAddonsDTO> resultProductAddons = productAddonsService.getDtoList(activeRecords, productVariantId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.list.message", null))
				.setData(resultProductAddons).create();
	}

	@PutMapping("/status/{productAddonsId}")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken, @RequestHeader("userId") final Long userId,
			@PathVariable("productAddonsId") final Long productAddonsId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of ProductAddons of id {} and status {}", productAddonsId, active);
		productAddonsService.changeStatus(productAddonsId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("addons.update.message", null))
				.create();
	}
}