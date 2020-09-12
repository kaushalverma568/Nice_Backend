package com.nice.controller;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.DiscountDTO;
import com.nice.dto.DiscountResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Discount;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DiscountService;

/**
 *
 * @author      : Kody Technolab PVT. LTD.
 * @date        : 23-Mar-2020
 * @description : Discount Related APIs
 */
@RequestMapping(path = "/discount")
@RestController
public class DiscountController {

	private static final Logger LOGGER = LoggerFactory.getLogger(DiscountController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private DiscountService discountService;

	/**
	 * Add Discount
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  discountDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Discount','CAN_ADD')")
	public ResponseEntity<Object> addDiscount(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final DiscountDTO discountDTO,
			final BindingResult bindingResult) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add discount {}", discountDTO);
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		discountService.addDiscount(discountDTO);
		LOGGER.info("Outside add discount ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("discount.create.message", null))
				.create();
	}

	/**
	 * Update Discount
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  discountDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Discount','CAN_EDIT')")
	public ResponseEntity<Object> updateDiscount(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final DiscountDTO discountDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update discount {}", discountDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Discount validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		discountService.updateDiscount(discountDTO);
		LOGGER.info("Outside update discount");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("discount.update.message", null))
				.create();
	}

	/**
	 * Get Discount Details based on id
	 *
	 * @param  discountId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{discountId}")
	public ResponseEntity<Object> getDiscount(@PathVariable("discountId") final Long discountId) throws NotFoundException, ValidationException {
		final DiscountResponseDTO discountResponseDTO = discountService.getDiscount(discountId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("discount.detail.message", null))
				.setData(discountResponseDTO).create();
	}

	/**
	 * Get Discount list based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  pincodeId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getDiscountListBasedOnParams(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "status", required = false) final String status, @RequestParam(name = "brandId", required = false) final Long brandId)
			throws ValidationException, NotFoundException {
		final Page<Discount> resultDiscounts = discountService.getDiscountListBasedOnParams(pageNumber, pageSize, status, brandId);
		List<DiscountResponseDTO> discountResponseDTOs = discountService.getDiscountListBasedOnParams(resultDiscounts.getContent());

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("discount.list.message", null))
				.setData(discountResponseDTOs).setHasNextPage(resultDiscounts.hasNext()).setHasPreviousPage(resultDiscounts.hasPrevious())
				.setTotalPages(resultDiscounts.getTotalPages()).setPageNumber(resultDiscounts.getNumber() + 1).setTotalCount(resultDiscounts.getTotalElements())
				.create();

	}

	/**
	 * Change status of discount (CANCELLED)
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  discountId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{discountId}")
	@PreAuthorize("hasPermission('Discount','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("discountId") final Long discountId,
			@RequestParam("status") final String status) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of discount for id {} and new status {}", discountId, status);
		discountService.changeStatus(discountId, status);
		LOGGER.info("Outside change status of discount ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("discount.update.message", null))
				.create();

	}

	/**
	 * Get product list of that discounted
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  pincodeId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/product/{discountId}")
	public ResponseEntity<Object> getProductListOfThatDiscount(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("discountId") final Long discountId) throws NotFoundException {
		Map<String, String> productMap = discountService.getProductListOfThatDiscount(discountId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("discount.list.message", null))
				.setData(productMap).create();
	}

}
