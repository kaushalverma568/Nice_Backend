package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

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

import com.nice.dto.ToppingDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ToppingMapper;
import com.nice.model.Topping;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ToppingService;
import com.nice.validator.ToppingValidator;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@RequestMapping(path = "/topping")
@RestController
public class ToppingController {
	/**
	 *
	 */
	private static final String LIST_MESSAGE = "topping.list.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ToppingController.class);
	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding topping
	 */
	@Autowired
	private ToppingValidator toppingValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(toppingValidator);
	}

	@Autowired
	private ToppingService toppingService;

	@Autowired
	private ToppingMapper toppingMapper;

	/**
	 * Add Topping
	 *
	 * @param toppingDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addTopping(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final ToppingDTO toppingDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Topping {}", toppingDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Topping validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}

		toppingService.addTopping(toppingDTO);
		LOGGER.info("Outside add Topping ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.create.message", null))
				.create();
	}

	/**
	 * update Topping
	 *
	 * @param toppingDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updateTopping(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final ToppingDTO toppingDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Topping {}", toppingDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Topping validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		toppingService.updateTopping(toppingDTO);
		LOGGER.info("Outside update Topping ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.update.message", null))
				.create();
	}

	/**
	 * Get Topping Details based on id
	 *
	 * @param toppingId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{toppingId}")
	public ResponseEntity<Object> getTopping(@RequestHeader("Authorization") final String accessToken, @PathVariable("toppingId") final Long toppingId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Topping ");
		final ToppingDTO resultToppingDTO = toppingService.getTopping(toppingId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.detail.message", null))
				.setData(resultToppingDTO).create();
	}

	/**
	 * Get topping list
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getToppingList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws NotFoundException {
		LOGGER.info("Inside get Topping List ");
		final Page<Topping> resultToppings = toppingService.getToppingList(pageNumber, pageSize, activeRecords, searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(LIST_MESSAGE, null))
				.setData(toppingMapper.toDtos(resultToppings.getContent())).setHasNextPage(resultToppings.hasNext())
				.setHasPreviousPage(resultToppings.hasPrevious()).setTotalPages(resultToppings.getTotalPages()).setPageNumber(resultToppings.getNumber() + 1)
				.setTotalCount(resultToppings.getTotalElements()).create();
	}

	/**
	 * Change status of topping (active/deActive)
	 *
	 * @param toppingId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{toppingId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("toppingId") final Long toppingId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of topping of id {} and status {}", toppingId, active);
		toppingService.changeStatus(toppingId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("topping.update.message", null))
				.create();
	}
}