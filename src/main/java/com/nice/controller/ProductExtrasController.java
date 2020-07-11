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

import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductExtrasService;
import com.nice.validator.ProductExtrasValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@RequestMapping(path = "/product/extras")
@RestController
public class ProductExtrasController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductExtrasController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductExtrasService productExtrasService;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private ProductExtrasValidator productExtrasValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(productExtrasValidator);
	}

	@PostMapping
	public ResponseEntity<Object> addProductExtras(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final ProductExtrasDTO productExtrasDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add ProductExtras {}", productExtrasDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductExtras validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long productExtrasId = productExtrasService.addProductExtras(productExtrasDTO);
		LOGGER.info("Outside add ProductExtras {}", productExtrasId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.create.message", null)).setData(productExtrasId).create();
	}

	@PutMapping
	public ResponseEntity<Object> updateProductExtras(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final ProductExtrasDTO productExtrasDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update ProductExtras {}", productExtrasDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ProductExtras validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long productExtrasId = productExtrasService.updateProductExtras(productExtrasDTO);
		LOGGER.info("Outside update ProductExtras {}", productExtrasId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.update.message", null)).setData(productExtrasId).create();
	}

	@GetMapping(value = "/{productExtrasId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken, @PathVariable("productExtrasId") final Long productExtrasId)
			throws NotFoundException {
		ProductExtrasDTO resultProductExtras = productExtrasService.getProductExtras(productExtrasId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.detail.message", null)).setData(resultProductExtras).create();
	}

	@GetMapping("/list/{productId}")
	public ResponseEntity<Object> getList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long productId,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) throws NotFoundException, ValidationException {
		final List<ProductExtrasDTO> resultProductExtras = productExtrasService.getListWithUserCheck(productId, activeRecords);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.extras.list.message", null))
				.setData(resultProductExtras).create();
	}

	@GetMapping("/cust/list/{productId}")
	public ResponseEntity<Object> getListForCustomer(@PathVariable final Long productId) throws NotFoundException {
		final List<ProductExtrasDTO> resultProductExtras = productExtrasService.getList(true, productId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.extras.list.message", null))
				.setData(resultProductExtras).create();
	}

	@PutMapping("/status/{productExtrasId}")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken, @RequestHeader("userId") final Long userId,
			@PathVariable("productExtrasId") final Long productExtrasId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of ProductExtras of id {} and status {}", productExtrasId, active);
		productExtrasService.changeStatus(productExtrasId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("product.extras.update.message", null)).create();
	}
}