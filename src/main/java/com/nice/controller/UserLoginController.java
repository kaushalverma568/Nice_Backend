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
import org.springframework.security.core.context.SecurityContextHolder;
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
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import com.nice.constant.Role;
import com.nice.constant.SuccessErrorType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.LoginResponse;
import com.nice.dto.PasswordDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UpdatePasswordParameterDTO;
import com.nice.dto.UserInfo;
import com.nice.dto.UserLoginDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.UserLogin;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.UserLoginService;
import com.nice.util.OauthTokenUtil;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
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
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ConsumerTokenServices consumerTokenServices;

	@Autowired
	private TokenStore tokenStore;

	@GetMapping("/forgotPassword")
	public ResponseEntity<Object> forgotPassword(@RequestBody @Valid final UpdatePasswordParameterDTO updatePasswordParameterDTO, final BindingResult result)
			throws ValidationException, NotFoundException, MessagingException {
		LOGGER.info("inside forgot password with UpdatePasswordParameterDTO : {}", updatePasswordParameterDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginService.forgotPassword(updatePasswordParameterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(
				updatePasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name()) ? "check.email.reset.password" : "check.message.reset.password",
				null)).create();
	}

	@PostMapping("/resetPassword/{email}/{otp}/{type}/{userType}")
	public ResponseEntity<Object> resetPassword(@PathVariable("email") final String email, @PathVariable("otp") final String otp,
			@RequestBody final String password, @PathVariable("type") final String type, @PathVariable("userType") final String userType)
			throws ValidationException, NotFoundException {

		String response = userLoginService.resetPassword(email, otp, password,
				UserOtpTypeEnum.EMAIL.name().equalsIgnoreCase(type) ? UserOtpTypeEnum.EMAIL.name() : UserOtpTypeEnum.SMS.name(), userType);
		return new GenericResponseHandlers.Builder().setMessage(response).setData(response).setStatus(HttpStatus.OK).create();
	}

	/**
	 * Get user info based on token
	 *
	 * @param accessToken
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(path = "/details")
	public ResponseEntity<Object> getUserInfo(@RequestHeader("Authorization") final String accessToken) throws NotFoundException {
		final String username = SecurityContextHolder.getContext().getAuthentication().getName();
		final UserInfo userLogin = userLoginService.getUserInfo(username);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(userLogin)
				.setMessage(messageByLocaleService.getMessage("user.login.detail.message", null)).create();
	}

	/**
	 * This method is use to verify the user.</br>
	 * We verify user through email only
	 *
	 * @param userId
	 * @param otp
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
	 * @param accessToken
	 * @param userId
	 * @param passwordDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping(path = "/change/password")
	public ResponseEntity<Object> changePassword(@RequestHeader("Authorization") final String accessToken, @RequestBody final PasswordDTO passwordDTO)
			throws NotFoundException, ValidationException {
		UserLogin userLogin = userLoginService.updatePassword(passwordDTO);
		/**
		 * When password is changed and the user is not super admin, revoke the user
		 * token
		 */
		if (!(Role.SUPER_ADMIN.name().equals(userLogin.getRole()))) {
			revokeToken(userLogin.getEmail());
		}

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("password.update", null)).create();
	}

	@GetMapping(path = "/logout")
	public ResponseEntity<Object> logout(@RequestHeader("Authorization") final String accessToken) {
		String tokenValue = accessToken.replace("Bearer", "").trim();
		consumerTokenServices.revokeToken(tokenValue);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("logout.message", null)).create();
	}

	/**
	 * Update Email For Admin
	 *
	 * @param accessToken
	 * @param userId
	 * @param passwordDTO
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
	 * Login using Facebook and Google. If User is not registered then we will add
	 * that user's information and if exists then will sent generated token.
	 *
	 * @param socialLoginDto
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	@PostMapping("/social")
	public ResponseEntity<Object> socialLogin(@RequestBody @Valid final SocialLoginDto socialLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info(" Inside social Login for email {} ", socialLoginDto.getEmail());

		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		SocialLoginDto resultSocialLoginDto = userLoginService.socialLogin(socialLoginDto);
		if (resultSocialLoginDto.isNewCustomer()) {
			userLoginService.sendWelComeEmail(resultSocialLoginDto.getUserId());
		}

		String url = ServletUriComponentsBuilder.fromCurrentContextPath().path("/").toUriString();
		LoginResponse loginResponse = OauthTokenUtil.getAuthToken(url, resultSocialLoginDto);
		loginResponse.setUserId(resultSocialLoginDto.getUserId());
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

	@PostMapping("/admin/login")
	public ResponseEntity<Object> adminLogin(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginDto.setUserType(UserType.USER.name());
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	@PostMapping("/customer/login")
	public ResponseEntity<Object> customerLogin(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginDto.setUserType(UserType.CUSTOMER.name());
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	@PostMapping("/app/login")
	public ResponseEntity<Object> deliveryBoyLogin(@RequestBody @Valid final UserLoginDto userLoginDto, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginDto.setUserType(UserType.DELIVERY_BOY.name());
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

}
