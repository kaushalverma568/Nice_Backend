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

import com.nice.response.GenericResponseHandlers;
import com.nice.dto.SubscriptionPlanDTO;
import com.nice.model.SubscriptionPlan;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SubscriptionPlanMapper;
import com.nice.service.SubscriptionPlanService;
import com.nice.validator.SubscriptionPlanValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@RequestMapping(path = "/subscription/plan")
@RestController
public class SubscriptionPlanController {

	private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionPlanController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private SubscriptionPlanService subscriptionPlanService;

	@Autowired
	private SubscriptionPlanMapper subscriptionPlanMapper;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private SubscriptionPlanValidator subscriptionPlanValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(subscriptionPlanValidator);
	}


	@PostMapping
	public ResponseEntity<Object> addSubscriptionPlan(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final SubscriptionPlanDTO subscriptionPlanDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add SubscriptionPlan {}", subscriptionPlanDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("SubscriptionPlan validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		SubscriptionPlanDTO resultSubscriptionPlan = subscriptionPlanService.addSubscriptionPlan(subscriptionPlanDTO);
		LOGGER.info("Outside add SubscriptionPlan {}", resultSubscriptionPlan);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("subscription.plan.create.message", null)).setData(resultSubscriptionPlan).create();
	}

	@PutMapping
	public ResponseEntity<Object> updateSubscriptionPlan(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final SubscriptionPlanDTO subscriptionPlanDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update SubscriptionPlan {}", subscriptionPlanDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("SubscriptionPlan validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		SubscriptionPlanDTO resultSubscriptionPlan = subscriptionPlanService.updateSubscriptionPlan(subscriptionPlanDTO);
		LOGGER.info("Outside update SubscriptionPlan {}", resultSubscriptionPlan);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("subscription.plan.update.message", null )).setData(resultSubscriptionPlan).create();
	}

	@GetMapping(value = "/{subscriptionPlanId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken, @PathVariable("subscriptionPlanId") final Long subscriptionPlanId)
			throws NotFoundException {
		SubscriptionPlanDTO resultSubscriptionPlan = subscriptionPlanService.getSubscriptionPlan(subscriptionPlanId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("subscription.plan.detail.message", null)).setData(resultSubscriptionPlan).create();
	}

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyWord ) {
		final Page<SubscriptionPlan> resultSubscriptionPlan = subscriptionPlanService.getList(pageNumber, pageSize, activeRecords,searchKeyWord);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("subscription.plan.list.message", null )).setData(subscriptionPlanMapper.toDtos(resultSubscriptionPlan.getContent()))
				.setHasNextPage(resultSubscriptionPlan.hasNext()).setHasPreviousPage(resultSubscriptionPlan.hasPrevious()).setTotalPages(resultSubscriptionPlan.getTotalPages())
				.setPageNumber(resultSubscriptionPlan.getNumber() + 1).setTotalCount(resultSubscriptionPlan.getTotalElements()).create();
	}

	@PutMapping("/status/{subscriptionPlanId}")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("subscriptionPlanId") final Long subscriptionPlanId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of SubscriptionPlan of id {} and status {}", subscriptionPlanId, active);
		subscriptionPlanService.changeStatus(subscriptionPlanId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("subscription.plan.update.message", null)).create();
	}
}