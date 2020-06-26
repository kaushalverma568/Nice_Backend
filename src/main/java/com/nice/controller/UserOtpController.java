package com.nice.controller;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.UserOtpDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.UserOtp;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.OtpService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@RestController
@RequestMapping("/otp")
public class UserOtpController {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OtpService otpService;

	private static final Logger LOGGER = LoggerFactory.getLogger(UserOtpController.class);

	/**
	 * @param  userLoginId
	 * @param  type
	 * @param  otp
	 * @param  email
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/verify")
	public ResponseEntity<Object> verifyOtp(@RequestParam(required = false) final Long userLoginId, @RequestParam(required = true) final String type,
			@RequestParam(required = true) final String otp, @RequestParam(required = false) final String email) throws ValidationException, NotFoundException {
		Boolean response = null;
		LOGGER.info("Inside verify OTP method");
		if (userLoginId != null) {
			response = otpService.verifyOtp(userLoginId, type, otp);
		} else if (email != null) {
			response = otpService.verifyOtp(email, type, otp);
		} else {
			LOGGER.error("Validation Exception as improper parameters specified");
			throw new ValidationException(messageByLocaleService.getMessage("user.login.or.email.mandatory", new Object[] {}));
		}
		LOGGER.info("OTP verification response : {}", response);
		return new GenericResponseHandlers.Builder().setData(response).setMessage(messageByLocaleService.getMessage("otp.verification", null))
				.setStatus(HttpStatus.OK).create();

	}

	/**
	 * @param  userOtpDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	@PostMapping
	public ResponseEntity<Object> generateOtp(@RequestBody @Valid final UserOtpDto userOtpDto, final BindingResult result)
			throws ValidationException, NotFoundException, MessagingException {
		LOGGER.info("Inside generate Otp : {}", userOtpDto);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		UserOtp otp = otpService.generateOtp(userOtpDto);
		otpService.sendOtp(userOtpDto, otp.getUserLogin(), otp.getOtp());
		LOGGER.info("Otp Generated Successfully, {}", userOtpDto);
		return new GenericResponseHandlers.Builder().setMessage("Otp Generated Successfully").setStatus(HttpStatus.OK).setData(otp.getOtp()).create();
	}

}
