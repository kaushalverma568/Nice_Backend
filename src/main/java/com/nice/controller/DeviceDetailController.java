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
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.DeviceDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DeviceDetailService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@RequestMapping(path = "/device/details")
@RestController
public class DeviceDetailController {

	private static final Logger LOGGER = LoggerFactory.getLogger(DeviceDetailController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private DeviceDetailService deviceDetailService;

	/**
	 * add/update device details
	 *
	 * @param accessToken
	 * @param userId
	 * @param deviceDetailDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	@PostMapping
	public ResponseEntity<Object> addDeviceDetail(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final DeviceDetailDTO deviceDetailDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Device detail {}", deviceDetailDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Device detail validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		deviceDetailService.addUpdateDeviceDetail(deviceDetailDTO);
		LOGGER.info("Outside add Device detail");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("device.detail.create.message", null)).create();
	}

}
