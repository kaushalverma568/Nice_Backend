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
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.SettingsDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.SettingsService;
import com.nice.validator.SettingsValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@RestController
@RequestMapping("/settings")
public class SettingsController {

	/**
	 *
	 */
	private static final String VALIDATION_FAILED_FOR_SETTINGS = "Validation Failed for Settings ";

	private static final String SETTING_DETAIL = "settings.detail.message";

	@Autowired
	private SettingsService settingsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private SettingsValidator settingsValidator;

	/**
	 * Binds the validator to the entity and returns the result in Binding Result
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(settingsValidator);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(SettingsController.class);

	/**
	 * @param  userId
	 * @param  settingsDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping
	public ResponseEntity<Object> addSettings(@RequestHeader(value = "Authorization") final String accessToken, @RequestBody @Valid SettingsDto settingsDto,
			final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add Setting method : {}", settingsDto);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VALIDATION_FAILED_FOR_SETTINGS);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		settingsDto = settingsService.addSettings(settingsDto);
		LOGGER.info("Added Setting Successfully : {}", settingsDto);
		return new GenericResponseHandlers.Builder().setData(settingsDto).setMessage(messageByLocaleService.getMessage("settings.create.message", null))
				.setStatus(HttpStatus.OK).create();
	}

	/**
	 * @param  userId
	 * @param  settingsDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 */
	@PutMapping
	public ResponseEntity<Object> updateSettings(@RequestHeader(value = "Authorization") final String accessToken,
			@RequestBody @Valid final SettingsDto settingsDto, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Setting method : {}", settingsDto);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VALIDATION_FAILED_FOR_SETTINGS);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		int count = settingsService.updateSettings(settingsDto);
		LOGGER.info("Updated Setting Successfully : {}", settingsDto);
		return new GenericResponseHandlers.Builder().setData("Total Setting updated : " + count)
				.setMessage(messageByLocaleService.getMessage("settings.update.message", null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * @return
	 */
	@GetMapping()
	public ResponseEntity<Object> getSettingsList(@RequestHeader(value = "Authorization") final String accessToken) {
		LOGGER.info("Inside getAll Settings list");
		List<SettingsDto> settingsDtoList = settingsService.getAllSettingsList();
		LOGGER.info("List fetched successfully, total objects returned : {}", settingsDtoList != null ? settingsDtoList.size() : 0);
		return new GenericResponseHandlers.Builder().setData(settingsDtoList).setMessage(messageByLocaleService.getMessage("settings.list.message", null))
				.setStatus(HttpStatus.OK).create();
	}

	/**
	 * @param  id
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping(value = "/{id}")
	public ResponseEntity<Object> getSettingsDetails(@RequestHeader(value = "Authorization") final String accessToken, @PathVariable final Long id)
			throws ValidationException {
		LOGGER.info("Get Setting for : {}", id);
		SettingsDto settingsDto = settingsService.getSettingsDetailsById(id);
		LOGGER.info("Details fetched successfully{}", settingsDto);
		return new GenericResponseHandlers.Builder().setData(settingsDto).setMessage(messageByLocaleService.getMessage(SETTING_DETAIL, null))
				.setStatus(HttpStatus.OK).create();
	}

	/**
	 * This method should be used only if we need to get the details of field value in decrypted format. Currently no need
	 * to use this method in front end.
	 *
	 * @param  fieldName
	 * @param  encrypted
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping(value = "/decrypted/{fieldName}/{encrypted}")
	public ResponseEntity<Object> getSettingsDetailsFromFieldName(@RequestHeader(value = "Authorization") final String accessToken,
			@PathVariable final String fieldName, @PathVariable final boolean encrypted) throws ValidationException, NotFoundException {
		LOGGER.info("Get Setting for field : {}", fieldName);

		/**
		 * The Below validation is place specifically so that no one can sniff the Secret of Payment Gateway using APIs, Hence
		 * deliberately we are throwing a not found message here.
		 */
		if ("PAYMENT_GATEWAY_SECRET".equalsIgnoreCase(fieldName)) {
			throw new ValidationException(messageByLocaleService.getMessage("settings.not.found.name", new Object[] { "PAYMENT_GATEWAY_SECRET" }));
		}

		SettingsDto settingsDto = encrypted ? settingsService.getSettingsDetailsByNameForEncryptedFields(fieldName)
				: settingsService.getSettingsDetailsByNameForNonEncryptedFields(fieldName);

		LOGGER.info("Details fetched successfully{}", settingsDto);
		return new GenericResponseHandlers.Builder().setData(settingsDto).setMessage(messageByLocaleService.getMessage(SETTING_DETAIL, null))
				.setStatus(HttpStatus.OK).create();
	}

	@GetMapping(value = "/fieldName/{fieldName}")
	public ResponseEntity<Object> getSettingsObject(@RequestHeader(value = "Authorization") final String accessToken, @PathVariable final String fieldName)
			throws ValidationException {
		LOGGER.info("Inside getAll Settings Details");
		SettingsDto settingsDto = settingsService.getSettingsDetailsByFieldName(fieldName);
		return new GenericResponseHandlers.Builder().setData(settingsDto).setMessage(messageByLocaleService.getMessage(SETTING_DETAIL, null))
				.setStatus(HttpStatus.OK).create();
	}

}
