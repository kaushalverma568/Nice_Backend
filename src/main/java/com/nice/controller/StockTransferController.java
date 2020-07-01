
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
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.StockTransferDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.StockTransferService;
import com.nice.validator.StockTransferValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jan-2020
 */
@RequestMapping(path = "/stockTransfer")
@RestController
public class StockTransferController {

	private static final Logger LOGGER = LoggerFactory.getLogger(StockTransferController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding state
	 */
	@Autowired
	private StockTransferValidator stockTransferValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(stockTransferValidator);
	}

	@Autowired
	private StockTransferService stockTransferService;

	/**
	 * Add State
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  addStockDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> transferStock(@RequestHeader("Authorization") final String accessToken, @RequestHeader("userId") final Long userId,
			@RequestBody @Valid final StockTransferDto stockTransferDto, final BindingResult bindingResult) throws ValidationException, NotFoundException {
		LOGGER.info("Inside manual stock transfer{}", stockTransferDto);
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
	     stockTransferService.transferStock(stockTransferDto);
		LOGGER.info("Stock Transferred successfully ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("transfer.message", null)).setData(null).create();
	}

}
