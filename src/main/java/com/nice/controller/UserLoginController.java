package com.nice.controller;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.ConsumerTokenServices;
import org.springframework.security.oauth2.provider.token.TokenStore;
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
import org.springframework.web.servlet.ModelAndView;

import com.nice.constant.RegisterVia;
import com.nice.constant.Role;
import com.nice.constant.SuccessErrorType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.ForgotPasswordParameterDTO;
import com.nice.dto.LoginResponse;
import com.nice.dto.PasswordDTO;
import com.nice.dto.ResetPasswordParameterDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UserLoginDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.UserLogin;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DeliveryBoyService;
import com.nice.service.UserLoginService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@RestController
@RequestMapping(value = "/user/login")
public class UserLoginController {

	private static final Logger LOGGER = LoggerFactory.getLogger(UserLoginController.class);

	private static final String REDIRECT = "redirect:";
	private static final String TYPE = "&type=";
	private static final String LOGIN_SUCCESS = "login.success";

	@Value("${customer.url}")
	private String customerUrl;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ConsumerTokenServices consumerTokenServices;

	@Autowired
	private TokenStore tokenStore;

	/**
	 * Generic Forgot password API
	 *
	 * @param  forgotPasswordParameterDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	@GetMapping("/forgotPassword")
	public ResponseEntity<Object> forgotPassword(@RequestBody @Valid final ForgotPasswordParameterDTO forgotPasswordParameterDTO, final BindingResult result)
			throws ValidationException, NotFoundException, MessagingException {
		LOGGER.info("inside forgot password with UpdatePasswordParameterDTO : {}", forgotPasswordParameterDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginService.forgotPassword(forgotPasswordParameterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(
				forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name()) ? "check.email.reset.password" : "check.message.reset.password",
				null)).create();
	}

	/**
	 * Reset password after forgot password based on OTP, USER TYPE and TYPE
	 *
	 * @param  email
	 * @param  otp
	 * @param  password
	 * @param  type
	 * @param  userType
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/resetPassword")
	public ResponseEntity<Object> resetPassword(@RequestBody @Valid final ResetPasswordParameterDTO resetPasswordParameterDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		String response = userLoginService.resetPassword(resetPasswordParameterDTO);
		return new GenericResponseHandlers.Builder().setMessage(response).setData(response).setStatus(HttpStatus.OK).create();
	}

	/**
	 * This method is use to verify the user.</br>
	 * We verify user through email only
	 *
	 * @param  userId
	 * @param  otp
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	@GetMapping("/verify/email/{userId}")
	public ModelAndView verifyEmail(@PathVariable("userId") final Long userId, @RequestParam(name = "otp") final String otp)
			throws ValidationException, NotFoundException {
		try {
			userLoginService.verifyUser(userId, otp);
			/**
			 * send email code starts from here
			 */
			userLoginService.sendWelComeEmail(userId);
			/**
			 * send email code ends from here
			 */
			String message = "Verifcation successfully";
			return new ModelAndView(REDIRECT + customerUrl + "thank-you?message=" + message + TYPE + SuccessErrorType.VERIFY_EMAIL);
		} catch (Exception e) {
			return new ModelAndView(REDIRECT + customerUrl + "failed-error?message=" + e.getMessage() + TYPE + SuccessErrorType.VERIFY_EMAIL);
		}
	}

	/**
	 * Change password for login user
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  passwordDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping(path = "/change/password")
	public ResponseEntity<Object> changePassword(@RequestHeader("Authorization") final String accessToken, @RequestBody final PasswordDTO passwordDTO)
			throws NotFoundException, ValidationException {
		UserLogin userLogin = userLoginService.updatePassword(passwordDTO);
		/**
		 * When password is changed and the user is not super admin, revoke the user token
		 */
		if (!(Role.SUPER_ADMIN.name().equals(userLogin.getRole()))) {
			revokeToken(userLogin.getEmail());
		}

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("password.update", null)).create();
	}

	/**
	 * Logout API : Also revoke access of token
	 *
	 * @param  accessToken
	 * @return
	 */
	@GetMapping(path = "/logout")
	public ResponseEntity<Object> logout(@RequestHeader("Authorization") final String accessToken) {
		String tokenValue = accessToken.replace("Bearer", "").trim();
		consumerTokenServices.revokeToken(tokenValue);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("logout.message", null)).create();
	}

	/**
	 * Update Email For Admin
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  passwordDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping(path = "/admin/{email}")
	public ResponseEntity<Object> updateEmailForAdmin(@RequestHeader("Authorization") final String accessToken, @PathVariable("email") final String email)
			throws ValidationException {
		LOGGER.info("Inside update email of admin method new email {}", email);
		userLoginService.updateEmailForAdmin(email);
		LOGGER.info("Outside update email of admin method");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("admin.emai.update.suceess", null))
				.create();
	}

	/**
	 * Login using Facebook and Google. If User is not registered then we will add that user's information and if exists
	 * then will sent generated token.
	 *
	 * @param  socialLoginDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/social")
	public ResponseEntity<Object> socialLogin(@RequestBody @Valid final SocialLoginDto socialLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		LOGGER.info(" Inside social Login for email {} ", socialLoginDto.getEmail());

		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		UserLoginDto userLoginDto = userLoginService.socialLogin(socialLoginDto);
		if (userLoginDto.isNewCustomer()) {
			userLoginService.sendWelComeEmail(userLoginDto.getUserId());
		}

		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	private void revokeToken(final String userName) {
		LOGGER.info("Revoking token for user {}", userName);
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName("kody-client", userName);
		for (OAuth2AccessToken token : tokens) {
			tokenStore.removeAccessToken(token);
		}
		LOGGER.info("Successfully Revoked token for user {}", userName);
	}

	/**
	 * ADMIN & USER Login and generate token
	 *
	 * @param  userLoginDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/admin/login")
	public ResponseEntity<Object> adminLogin(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		LoginResponse loginResponse = userLoginService.adminLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	/**
	 * Customer Login and generate token
	 *
	 * @param  userLoginDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/customer/login")
	public ResponseEntity<Object> customerLogin(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginDto.setUserType(UserType.CUSTOMER.name());
		userLoginDto.setRegisteredVia(RegisterVia.APP.getStatusValue());
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	/**
	 * Delivery boy Login and generate token
	 *
	 * @param  userLoginDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/delivery/boy/login")
	public ResponseEntity<Object> deliveryBoyLogin(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginDto.setUserType(UserType.DELIVERY_BOY.name());
		userLoginDto.setRegisteredVia(RegisterVia.APP.getStatusValue());
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		/**
		 * update is login flag to true when delivery boy successfully logged in
		 */
		deliveryBoyService.updateIsLogin(userLoginDto.getUserName());
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	/**
	 * Login with OTP for customer
	 *
	 * @param  userLoginDto
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/customer/login/otp")
	public ResponseEntity<Object> customerLoginOtp(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginDto.setUserType(UserType.CUSTOMER.name());
		userLoginDto.setRegisteredVia(RegisterVia.OTP.getStatusValue());
		userLoginService.checkOtpForLogin(userLoginDto);
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	/**
	 * API is useful for generate OTP for login. </br>
	 * If customer is not exist with respect to mobile then it will create customer based on phoneNumber.
	 *
	 * @param  phoneNumber
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/customer/generate/otp/{phoneNumber}")
	public ResponseEntity<Object> generateOtpForLogin(@PathVariable(name = "phoneNumber") final String phoneNumber)
			throws ValidationException, NotFoundException {
		String otp = userLoginService.generateOtpForLogin(phoneNumber);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("otp.generated.success", null)).setData(otp)
				.setStatus(HttpStatus.OK).create();
	}

	/**
	 * check password for user
	 *
	 * @param  entityId
	 * @param  entityType
	 * @param  password
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/check/password/{entityId}/{entityType}")
	public ResponseEntity<Object> checkPasswordForUser(@PathVariable(name = "entityId") final Long entityId,
			@PathVariable(name = "entityType") final String entityType, @RequestParam(name = "password", required = true) final String password)
			throws ValidationException {
		userLoginService.checkPasswordForUser(entityId, entityType, password);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("password.match.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

}
