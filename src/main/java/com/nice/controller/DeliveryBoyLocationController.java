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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.DeliveryBoyLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyLocationMapper;
import com.nice.model.DeliveryBoyLocation;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DeliveryBoyLocationService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Dec-2019
 */
@RequestMapping(path = "/deliveryboy/location")
@RestController
public class DeliveryBoyLocationController {

	private static final Logger LOGGER = LoggerFactory.getLogger(DeliveryBoyLocationController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	/**
	 * service - to implement business logic
	 */
	@Autowired
	private DeliveryBoyLocationService deliveryBoyLocationService;

	@Autowired
	private DeliveryBoyLocationMapper deliveryBoyLocationMapper;

	/**
	 * Add DeliveryBoyLocation
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  deliveryBoyLocationDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addDeliveryBoyLocation(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final DeliveryBoyLocationDTO deliveryBoyLocationDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add delivery boy location {}", deliveryBoyLocationDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("delivery boy location validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		deliveryBoyLocationService.addUpdateDeliveryBoyLocation(deliveryBoyLocationDTO);
		LOGGER.info("Outside add deliveryBoyLocation");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("deliveryboy.location.create.message", null)).create();
	}

	/**
	 * Get DeliveryBoyLocation Details based on id
	 *
	 * @param  deliveryBoyLocationId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{deliveryBoyLocationId}")
	public ResponseEntity<Object> getDeliveryBoyLocation(@PathVariable("deliveryBoyLocationId") final Long deliveryBoyLocationId) throws NotFoundException {
		final DeliveryBoyLocationDTO resultDeliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLocation(deliveryBoyLocationId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("deliveryboy.location.detail.message", null)).setData(resultDeliveryBoyLocation).create();
	}

	/**
	 * Get delivery boy's Location Details by delivery boy id
	 *
	 * @param  deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/latest/{deliveryBoyId}")
	public ResponseEntity<Object> getDeliveryBoyLocationByDeliveryBoyId(@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException {
		final DeliveryBoyLocation resultDeliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLocationByDeliveryBoyId(deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("deliveryboy.location.detail.message", null))
				.setData(deliveryBoyLocationMapper.toDto(resultDeliveryBoyLocation)).create();
	}
}
