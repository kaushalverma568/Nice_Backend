package com.nice.controller;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
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

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.RegisterVia;
import com.nice.constant.Role;
import com.nice.constant.SuccessErrorType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.EmailUpdateDTO;
import com.nice.dto.ForgotPasswordParameterDTO;
import com.nice.dto.LoginEmailDTO;
import com.nice.dto.LoginOtpDTO;
import com.nice.dto.LoginResponse;
import com.nice.dto.PasswordDTO;
import com.nice.dto.ResetPasswordParameterDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UserCheckPasswordDTO;
import com.nice.dto.UserLoginDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.UserLogin;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@RestController
@RequestMapping(value = "/user/login")
public class UserLoginController {

	/**
	 *
	 */
	private static final String VERIFY_USER_SUCCESS = "verify.user.success";

	private static final Logger LOGGER = LoggerFactory.getLogger(UserLoginController.class);

	private static final String REDIRECT = "redirect:";
	private static final String TYPE = "&type=";
	private static final String LOGIN_SUCCESS = "login.success";

	@Value("${customer.url}")
	private String customerUrl;

	@Value("${admin.url}")
	private String adminUrl;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ConsumerTokenServices consumerTokenServices;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private TokenStore tokenStore;

	/**
	 * Generic Forgot password API
	 *
	 * @param forgotPasswordParameterDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	@PostMapping("/forgotPassword")
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
	 * @param email
	 * @param otp
	 * @param password
	 * @param type
	 * @param userType
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
		return new GenericResponseHandlers.Builder().setMessage(response).setStatus(HttpStatus.OK).create();
	}

	/**
	 * Verify email by userLogin Id
	 *
	 * @param userId
	 * @param otp
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnsupportedEncodingException
	 */
	@GetMapping("/verify/email/{userId}")
	public ModelAndView verifyEmail(@PathVariable("userId") final Long userId, @RequestParam(name = "otp") final String otp,
			@RequestParam(name = "lang") final String lang) throws ValidationException, NotFoundException, UnsupportedEncodingException {
		Locale locale = new Locale(lang);
		LocaleContextHolder.setLocale(locale);
		String redirectUrl;
		UserLogin userLogin = userLoginService.getUserLoginDetail(userId);
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			redirectUrl = customerUrl;
		} else {
			redirectUrl = adminUrl;
		}
		try {
			userLoginService.verifyEmail(userId, otp, null);
			/**
			 * send email code starts from here
			 */
			if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
				userLoginService.sendWelComeEmail(userId);
			}
			/**
			 * send email code ends from here
			 */

			/**
			 * send push notification for new vendor to admin
			 */
			if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
				userLoginService.sendPushNotificationForNewProfile(userLogin.getEntityId(), userLogin.getEntityType());
			}
			String message = messageByLocaleService.getMessage("verify.email.success", null);
			return new ModelAndView(REDIRECT + redirectUrl + "auth/thank-you?message="
					+ URLEncoder.encode(messageByLocaleService.getMessage(message, null), "UTF-8") + TYPE + SuccessErrorType.VERIFY_EMAIL);
		} catch (Exception e) {
			return new ModelAndView(REDIRECT + redirectUrl + "auth/failed-error?message="
					+ URLEncoder.encode(messageByLocaleService.getMessage(e.getMessage(), null), "UTF-8") + TYPE + SuccessErrorType.VERIFY_EMAIL);
		}
	}

	/**
	 * verify email by userName(email) and userType
	 *
	 * @param resetPasswordParameterDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/verify/email/otp")
	public ResponseEntity<Object> verifyEmailByUserNameAndUserType(@RequestBody @Valid final ResetPasswordParameterDTO resetPasswordParameterDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long userId = userLoginService.verifyEmail(null, null, resetPasswordParameterDTO);
		/**
		 * send email code starts from here
		 */
		userLoginService.sendWelComeEmail(userId);
		/**
		 * send email code ends from here
		 */
		UserLogin userLogin = userLoginService.getUserLoginDetail(userId);
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VERIFY_USER_SUCCESS, null))
					.setData(customerService.getCustomer(userLogin.getEntityId())).create();
		} else if (UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			/**
			 * send push notification
			 */
			userLoginService.sendPushNotificationForNewProfile(userLogin.getEntityId(), userLogin.getEntityType());
			return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VERIFY_USER_SUCCESS, null))
					.setData(deliveryBoyService.getDeliveryBoy(userLogin.getEntityId())).create();
		} else if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			/**
			 * send push notification
			 */
			userLoginService.sendPushNotificationForNewProfile(userLogin.getEntityId(), userLogin.getEntityType());
			return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VERIFY_USER_SUCCESS, null))
					.setData(vendorService.getVendor(userLogin.getEntityId())).create();
		} else {
			return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VERIFY_USER_SUCCESS, null))
					.create();
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
		if (!(Role.SUPER_ADMIN.getStatusValue().equals(userLogin.getRole().getName()))) {
			revokeToken(userLogin.getEmail());
		}

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("password.update", null)).create();
	}

	/**
	 * Logout API : Also revoke access of token
	 *
	 * @param accessToken
	 * @return
	 */
	@GetMapping(path = "/logout")
	public ResponseEntity<Object> logout(@RequestHeader("Authorization") final String accessToken) {
		String tokenValue = accessToken.replace("Bearer", "").trim();
		consumerTokenServices.revokeToken(tokenValue);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("logout.message", null)).create();
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
	 * @param userLoginDto
	 * @param result
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
	 * @param userLoginDto
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/customer/login")
	public ResponseEntity<Object> customerLogin(@RequestBody @Valid final LoginEmailDTO loginEmailDTO, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		UserLoginDto userLoginDto = new UserLoginDto();
		userLoginDto.setUserName(loginEmailDTO.getEmail());
		userLoginDto.setPassword(loginEmailDTO.getPassword());
		userLoginDto.setUserType(UserType.CUSTOMER.name());
		userLoginDto.setRegisteredVia(RegisterVia.APP.getStatusValue());
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	/**
	 * Delivery boy Login and generate token
	 *
	 * @param userLoginDto
	 * @param result
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
		 * if at a time same delivery boy is login more than once then revoke all tokens
		 * other then this
		 */
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName(Constant.CLIENT_ID,
				userLoginDto.getUserName().concat("!!").concat(UserType.DELIVERY_BOY.name()));
		Collection<OAuth2AccessToken> removedTokens = new ArrayList<>();
		for (OAuth2AccessToken token : tokens) {
			if (!token.toString().equals(loginResponse.getAccessToken())) {
				removedTokens.add(token);
			}
		}
		for (OAuth2AccessToken token : removedTokens) {
			deliveryBoyService.validateBeforeLogout();
			tokenStore.removeAccessToken(token);
		}

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
	 * @param userLoginDto
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PostMapping("/customer/login/otp")
	public ResponseEntity<Object> customerLoginOtp(@RequestBody @Valid final LoginOtpDTO loginOtpDTO, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		UserLoginDto userLoginDto = new UserLoginDto();
		userLoginDto.setUserName(loginOtpDTO.getPhoneNumber());
		userLoginDto.setPassword(loginOtpDTO.getOtp());
		userLoginDto.setUserType(UserType.CUSTOMER.name());
		userLoginDto.setRegisteredVia(RegisterVia.OTP.getStatusValue());
		userLoginService.checkOtpForLogin(userLoginDto);
		LoginResponse loginResponse = userLoginService.checkUserLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage(LOGIN_SUCCESS, null)).create();
	}

	/**
	 * API is useful for generate OTP for login. </br>
	 * If customer is not exist with respect to mobile then it will create customer
	 * based on phoneNumber.
	 *
	 * @param phoneNumber
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
	 * @param accessToken
	 * @param entityId
	 * @param entityType
	 * @param password
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping("/check/password")
	public ResponseEntity<Object> checkPasswordForUser(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final UserCheckPasswordDTO userCheckPasswordDTO, final BindingResult result) throws ValidationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		userLoginService.checkPasswordForUser(userCheckPasswordDTO);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("password.match.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 * Add/Update email
	 *
	 * @param accessToken
	 * @param customerId
	 * @param phoneNumber
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PutMapping("/email")
	public ResponseEntity<Object> addUpdateEmail(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final EmailUpdateDTO emailUpdateDTO, final BindingResult result)
			throws ValidationException, NotFoundException, UnAuthorizationException {
		LOGGER.info("Inside add update email {} and otp: {}", emailUpdateDTO.getEmail(), emailUpdateDTO.getOtp());
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Customers validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		String userName = userLoginService.addUpdateEmail(emailUpdateDTO, userLogin);
		/**
		 * for super admin we have to use USER as userType
		 */
		if (Role.SUPER_ADMIN.getStatusValue().equals(userLogin.getRole().getName())) {
			userLogin.setEntityType(UserType.USER.name());
		}
		LoginResponse loginResponse = null;
		if (userName != null) {
			revokeToken(userName.concat("!!").concat(userLogin.getEntityType()));
			UserLoginDto userLoginDto = new UserLoginDto();
			userLoginDto.setUserName(emailUpdateDTO.getEmail());
			userLoginDto.setUserType(userLogin.getEntityType());
			userLoginDto.setPassword(emailUpdateDTO.getPassword());
			userLoginDto.setRegisteredVia(RegisterVia.APP.getStatusValue());
			loginResponse = userLoginService.checkUserLogin(userLoginDto);
		} else {
			LOGGER.error("username not found");
		}
		LOGGER.info("Outside  add update email");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage("emai.update.success", null)).create();
	}

	/**
	 * Add/Update phone number
	 *
	 * @param accessToken
	 * @param customerId
	 * @param phoneNumber
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	@PutMapping("/phone")
	public ResponseEntity<Object> addUpdatePhoneNumber(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "otp", required = true) final String otp, @RequestParam(name = "phoneNumber", required = true) final String phoneNumber,
			@RequestParam(name = "userType", required = true) final String userType) throws ValidationException, NotFoundException, UnAuthorizationException {
		LOGGER.info("Inside add update PhoneNumber {} and otp: {} and userType: {}", phoneNumber, otp, userType);
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		String userName = userLoginService.addUpdatePhoneNumber(phoneNumber, otp, userType, userLogin);
		/**
		 * for super admin we have to use USER as userType
		 */
		if (Role.SUPER_ADMIN.getStatusValue().equals(userLogin.getRole().getName())) {
			userLogin.setEntityType(UserType.USER.name());
		}
		LoginResponse loginResponse = null;
		if (userName != null) {
			revokeToken(userName.concat("!!").concat(userLogin.getEntityType()));
			String loginOtp = userLoginService.generateOtpForLogin(phoneNumber);
			UserLoginDto userLoginDto = new UserLoginDto();
			userLoginDto.setUserName(userName);
			userLoginDto.setUserType(userLogin.getEntityType());
			userLoginDto.setPassword(loginOtp);
			userLoginDto.setRegisteredVia(RegisterVia.OTP.getStatusValue());
			loginResponse = userLoginService.checkUserLogin(userLoginDto);
		} else {
			LOGGER.error("username not found");
		}
		LOGGER.info("Outside add update PhoneNumber");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(loginResponse)
				.setMessage(messageByLocaleService.getMessage("phone.update.success", null)).create();
	}

	/**
	 * Get user info based on token
	 *
	 * @param accessToken
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping(path = "/basic")
	public ResponseEntity<Object> getUserInfo(@RequestHeader("Authorization") final String accessToken) throws NotFoundException {
		LoginResponse userInfo = userLoginService.getUserInfo();
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(userInfo)
				.setMessage(messageByLocaleService.getMessage("users.detail.message", null)).create();
	}
}
