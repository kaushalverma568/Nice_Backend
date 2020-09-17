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

import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderRatingMapper;
import com.nice.model.OrderRating;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.OrderRatingService;
import com.nice.validator.OrderRatingValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@RequestMapping(path = "/order/rating")
@RestController
public class OrderRatingController {

	private static final Logger LOGGER = LoggerFactory.getLogger(OrderRatingController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	private static final String DETAIL_MSG = "order.rating.detail.message";

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrderRatingService orderRatingService;

	@Autowired
	private OrderRatingMapper orderRatingMapper;

	@Autowired
	private OrderRatingValidator orderRatingValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(orderRatingValidator);
	}

	
    /**
     *  add ratings to order
     *  
     * @param accessToken
     * @param orderRatingDTO
     * @param result
     * @return
     * @throws ValidationException
     * @throws NotFoundException
     */
	@PostMapping
	public ResponseEntity<Object> addOrderRating(@RequestHeader("Authorization") final String accessToken, 
			@RequestBody @Valid final OrderRatingDTO orderRatingDTO, final BindingResult result) 
					throws ValidationException, NotFoundException {
		LOGGER.info("Inside add OrderRating {}", orderRatingDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("OrderRating validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		OrderRatingResponseDTO resultOrderRating = orderRatingService.addOrderRating(orderRatingDTO);
		LOGGER.info("Outside add OrderRating {}", resultOrderRating);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("order.rating.create.message", null)).setData(resultOrderRating).create();
	}

	/**
	 * get by id 
	 * 
	 * @param accessToken
	 * @param orderRatingId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(value = "/{orderRatingId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken, 
			@PathVariable("orderRatingId") final Long orderRatingId)
			throws NotFoundException {
		OrderRatingResponseDTO resultOrderRating = orderRatingService.getOrderRating(orderRatingId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(DETAIL_MSG, null)).setData(resultOrderRating).create();
	}
	

	/**
	 * get by order id 
	 * 
	 * @param accessToken
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(value = "/order/{orderId}")
	public ResponseEntity<Object> getByOrderId(@RequestHeader("Authorization") final String accessToken, 
			@PathVariable("orderId") final Long orderId)
			throws NotFoundException {
		OrderRatingResponseDTO resultOrderRating = orderRatingService.getOrderRatingbyOrderId(orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(DETAIL_MSG, null)).setData(resultOrderRating).create();
	}

	
	/**
	 * get list by vendor ID 
	 * 
	 * @param accessToken
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(value = "/vendor/{vendorId}/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getByVendorId(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestHeader("Authorization") final String accessToken, 
			@PathVariable("vendorId") final Long vendorId)
			throws NotFoundException {
		List<OrderRating> resultOrderRating = orderRatingService.getOrderRatingByVendorId(pageNumber, pageSize, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(DETAIL_MSG, null))
				.setData(orderRatingMapper.toResponseDtos(resultOrderRating)).create();
	}

	
	@GetMapping(value = "/deliveryBoy/{deliveryBoyId}/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getBydeliveryBoyId(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestHeader("Authorization") final String accessToken, 
			@PathVariable("deliveryBoyId") final Long deliveryBoyId)
			throws NotFoundException {
		List<OrderRating> resultOrderRating = orderRatingService.getOrderRatingByDeliveryBoyId(pageNumber, pageSize, deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(DETAIL_MSG, null))
				.setData(orderRatingMapper.toResponseDtos(resultOrderRating)).create();
	}

	
	/**
	 * get list by pagination 
	 * 
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyWord
	 * @return
	 * @throws NotFoundException 
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyWord) throws NotFoundException {
		final Page<OrderRating> resultOrderRating = orderRatingService.getList(pageNumber, pageSize, activeRecords,searchKeyWord);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("order.rating.list.message", null)).setData(orderRatingMapper.toResponseDtos(resultOrderRating.getContent()))
				.setHasNextPage(resultOrderRating.hasNext()).setHasPreviousPage(resultOrderRating.hasPrevious()).setTotalPages(resultOrderRating.getTotalPages())
				.setPageNumber(resultOrderRating.getNumber() + 1).setTotalCount(resultOrderRating.getTotalElements()).create();
	}

	/**
	 *  for active de-acvtive record
	 *  
	 * @param accessToken
	 * @param orderRatingId
	 * @param active
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/status/{orderRatingId}")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken, 
			@PathVariable("orderRatingId") final Long orderRatingId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of OrderRating of id {} and status {}", orderRatingId, active);
		orderRatingService.changeStatus(orderRatingId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("order.rating.update.message", null)).create();
	}
}