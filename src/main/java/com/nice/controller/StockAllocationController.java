/**
 *
 */
package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

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

import com.nice.dto.StockAllocationDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.response.GenericResponseHandlers;
import com.nice.validator.StockAllocationValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@RestController
@RequestMapping("/allocate/stock")
public class StockAllocationController {

	@Autowired
	private StockAllocationValidator stockAllocationValidator;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(stockAllocationValidator);
	}

	@PostMapping
	public ResponseEntity<Object> allocateStock(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final StockAllocationDto stockAllocationDto, final BindingResult bindingResult) throws NotFoundException, ValidationException {
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}

		return new GenericResponseHandlers.Builder().setMessage("Stock Allocated Successfully").setStatus(HttpStatus.OK).create();
	}
}
