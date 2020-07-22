package com.nice.service.impl;

import java.util.Arrays;
import java.util.Optional;

import javax.mail.MessagingException;

import org.apache.commons.codec.binary.Base64;
import org.apache.http.impl.client.HttpClients;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCrypt;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.CustomerStatus;
import com.nice.constant.DeliveryBoyStatus;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.RegisterVia;
import com.nice.constant.Role;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.dto.CustomerDTO;
import com.nice.dto.EmailUpdateDTO;
import com.nice.dto.ForgotPasswordParameterDTO;
import com.nice.dto.LoginResponse;
import com.nice.dto.Notification;
import com.nice.dto.PasswordDTO;
import com.nice.dto.ResetPasswordParameterDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UserLoginDto;
import com.nice.dto.UserOtpDto;
import com.nice.exception.BaseRuntimeException;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyCurrentStatus;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Users;
import com.nice.model.Vendor;
import com.nice.repository.CustomerRepository;
import com.nice.repository.DeliveryBoyRepository;
import com.nice.repository.UserLoginRepository;
import com.nice.repository.UsersRepository;
import com.nice.repository.VendorRepository;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OtpService;
import com.nice.service.UserLoginService;
import com.nice.service.UsersService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Service(value = "userLoginService")
@Transactional(rollbackFor = Throwable.class)
public class UserLoginServiceImpl implements UserLoginService, UserDetailsService {

	/**
	 * 
	 */
	private static final String VENDOR_ACTIVE_FIRST = "vendor.active.first";

	/**
	 *
	 */
	private static final String USER_EMAIL_NOT_ACTIVATE = "user.email.not.activate";

	/**
	 *
	 */
	private static final String USER_ACCOUNT_UNAUTHORIZED_ADMIN = "user.account.unauthorized.admin";

	/**
	 *
	 */
	private static final String INVALID_USER_TYPE = "invalid.user.type";

	/**
	 *
	 */
	private static final String CUSTOMER_NOT_FOUND_PHONE = "customer.not.found.phone";

	/**
	 *
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UserLoginServiceImpl.class);

	@Autowired
	private UserLoginRepository userLoginRepository;

	@Autowired
	private OtpService otpService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Autowired
	private CustomerRepository customerRepository;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private DeliveryBoyRepository deliveryBoyRepository;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private VendorRepository vendorRepository;

	@Autowired
	private UsersService usersService;

	@Autowired
	private UsersRepository usersRepository;

	@SuppressWarnings("unused")
	@Override
	public UserDetails loadUserByUsername(final String username) {
		String actualUser = null;
		String actualUserWithType = null;
		String requestVia = RegisterVia.APP.getStatusValue();
		String userType = null;
		if (username != null && username.contains("#")) {
			actualUserWithType = username.split("#")[0];
			requestVia = username.split("#")[1];
		} else {
			actualUserWithType = username;
		}

		/**
		 * Check if the userame contains the role , if not throw error
		 */

		if (actualUserWithType != null && actualUserWithType.contains("!!")) {
			actualUser = actualUserWithType.split("!!")[0];
			userType = actualUserWithType.split("!!")[1];
		} else {
			throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage("specify.role", new Object[] {}));
		}

		Optional<UserLogin> optUserLogin;

		if (RegisterVia.OTP.getStatusValue().equals(requestVia)) {
			optUserLogin = userLoginRepository.findByPhoneNumberIgnoreCaseAndEntityType(actualUser, userType);
		}
		/**
		 * Fetch user based on email(userName)
		 */
		else {
			optUserLogin = userLoginRepository.findByEmailAndEntityType(actualUser, userType);
		}
		/**
		 * If the userType is USERS and optUserLogin is empty, the user might be a superadmin, check if the user is superadmin.
		 */
		if (!optUserLogin.isPresent() && UserType.USER.name().equalsIgnoreCase(userType)) {
			optUserLogin = userLoginRepository.findByEmailAndRole(actualUser, Role.SUPER_ADMIN.name());
		}

		/**
		 * If user is not exists then throw an error
		 */
		if (!optUserLogin.isPresent()) {
			throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage("invalid.username", null));
		}
		/**
		 * If user is not active then possible 2 cases </br>
		 * 1.User is not activated yet. </br>
		 * 2.User is deactivated by Administrator </br>
		 */
		else if (!optUserLogin.get().getActive().booleanValue()) {
			if (optUserLogin.get().getEntityType().equals(Role.CUSTOMER.getStatusValue())) {
				Customer customer = null;
				try {
					customer = customerService.getCustomerDetails(optUserLogin.get().getEntityId());
				} catch (final NotFoundException e) {
					LOGGER.error("Customer not found for customer Id: {} ", optUserLogin.get().getEntityId());
				}
				/**
				 * If user status is deactivate: then send related message
				 */
				if ((customer != null) && customer.getStatus().equals(CustomerStatus.DE_ACTIVE.getStatusValue())) {
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage(USER_ACCOUNT_UNAUTHORIZED_ADMIN, null));
				} else {
					/**
					 * If customer is not activated yet then send related message
					 */
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED,
							messageByLocaleService.getMessage(USER_EMAIL_NOT_ACTIVATE, new Object[] { actualUser }));
				}
			} else if (optUserLogin.get().getEntityType().equals(Role.DELIVERY_BOY.getStatusValue())) {
				DeliveryBoy deliveryBoy = null;
				try {
					deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(optUserLogin.get().getEntityId());
				} catch (final NotFoundException e) {
					LOGGER.error("Delivery boy not found for delivery boy Id: {} ", optUserLogin.get().getEntityId());
				}
				/**
				 * If user status is deactivate: then send related message
				 */
				if ((deliveryBoy != null) && deliveryBoy.getStatus().equals(DeliveryBoyStatus.DE_ACTIVE.getStatusValue())) {
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage(USER_ACCOUNT_UNAUTHORIZED_ADMIN, null));
				} else {
					/**
					 * If customer is not activated yet then send related message
					 */
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED,
							messageByLocaleService.getMessage(USER_EMAIL_NOT_ACTIVATE, new Object[] { actualUser }));
				}
			} else if (optUserLogin.get().getEntityType().equals(Role.VENDOR.getStatusValue())) {
				Vendor vendor = null;
				try {
					vendor = vendorService.getVendorDetail(optUserLogin.get().getEntityId());
				} catch (final NotFoundException e) {
					LOGGER.error("Vendor not found for vendor Id: {} ", optUserLogin.get().getEntityId());
				}
				/**
				 * If user status is deactivate: then send related message
				 */
				if ((vendor != null) && vendor.getStatus().equals(VendorStatus.VERIFICATION_PENDING.getStatusValue())) {
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED,
							messageByLocaleService.getMessage(USER_EMAIL_NOT_ACTIVATE, new Object[] { actualUser }));
				} else {
					LOGGER.info("As per requirement we will allowed vendor to login when vednor deactive");
				}
			} else {
				throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage(USER_ACCOUNT_UNAUTHORIZED_ADMIN, null));
			}
		} else {
			/**
			 * This case possible when first login with OTP and then sign-up with email + mobile. In this case userLogin can be
			 * active but customer can not login with email and password but it can login with OTP.
			 */
			if (optUserLogin.get().getEntityType() != null && optUserLogin.get().getEntityType().equals(Role.CUSTOMER.getStatusValue())
					&& !RegisterVia.OTP.getStatusValue().equals(requestVia)) {
				Customer customer = null;
				try {
					customer = customerService.getCustomerDetails(optUserLogin.get().getEntityId());
				} catch (final NotFoundException e) {
					LOGGER.error("Customer not found for customer Id: {} ", optUserLogin.get().getEntityId());
				}
				if (customer != null && (customer.getEmailVerified() == null || !customer.getEmailVerified())) {
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED,
							messageByLocaleService.getMessage(USER_EMAIL_NOT_ACTIVATE, new Object[] { actualUser }));
				}
			}
		}

		final String role = optUserLogin.get().getRole();
		final SimpleGrantedAuthority authority = new SimpleGrantedAuthority("ROLE_" + role);
		if (RegisterVia.GOOGLE.getStatusValue().equals(requestVia)) {
			return new UserAwareUserDetails(actualUserWithType, optUserLogin.get().getGoogleKey(), Arrays.asList(authority), optUserLogin.get());
		} else if (RegisterVia.FACEBOOK.getStatusValue().equals(requestVia)) {
			return new UserAwareUserDetails(actualUserWithType, optUserLogin.get().getFacebookKey(), Arrays.asList(authority), optUserLogin.get());
		} else if (RegisterVia.OTP.getStatusValue().equals(requestVia)) {
			return new UserAwareUserDetails(actualUserWithType, optUserLogin.get().getOtp(), Arrays.asList(authority), optUserLogin.get());
		} else {
			if (optUserLogin.get().getPassword() == null) {
				throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage("user.unauthorized.social", null));
			}
			return new UserAwareUserDetails(actualUserWithType, optUserLogin.get().getPassword(), Arrays.asList(authority), optUserLogin.get());
		}
	}

	@Override
	public UserLogin addUserLogin(final UserLogin userLogin) throws NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getFacebookKey())) {
			userLogin.setFacebookKey(CommonUtility.generateBcrypt(userLogin.getFacebookKey()));
		} else if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getGoogleKey())) {
			userLogin.setGoogleKey(CommonUtility.generateBcrypt(userLogin.getGoogleKey()));
		} else if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getOtp())) {
			userLogin.setOtp(CommonUtility.generateBcrypt(userLogin.getOtp()));
		} else {
			userLogin.setPassword(CommonUtility.generateBcrypt(userLogin.getPassword()));
		}
		return userLoginRepository.save(userLogin);
	}

	@Override
	public UserLogin updateUserLogin(final UserLogin userLogin) throws NotFoundException {
		return userLoginRepository.save(userLogin);
	}

	@Override
	public Optional<UserLogin> getUserLogin(final Long userId) {
		return userLoginRepository.findById(userId);
	}

	@Override
	public Optional<UserLogin> getUserLoginBasedOnEmailAndEntityType(final String email, final String entityType) {
		return userLoginRepository.findByEmailAndEntityType(email, entityType);
	}

	@Override
	public UserLogin getUserLoginDetail(final Long userId) throws NotFoundException {
		return userLoginRepository.findById(userId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userId })));
	}

	private void sendForgotPasswordLink(final String otp, final String email, final String userType, final String sendingType) {
		final Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setEmail(email);
		notification.setUserType(userType);
		notification.setSendingType(sendingType);
		notification.setType(NotificationQueueConstants.FORGOT_PASS);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	@Override
	public UserLoginDto socialLogin(final SocialLoginDto socialLoginDto) throws ValidationException, NotFoundException {
		UserLoginDto userLoginDto = new UserLoginDto();
		userLoginDto.setUserName(socialLoginDto.getEmail().toLowerCase());
		userLoginDto.setPassword(socialLoginDto.getUniqueId());
		userLoginDto.setRegisteredVia(socialLoginDto.getRegisteredVia());
		userLoginDto.setUserType(Role.CUSTOMER.getStatusValue());
		final Optional<UserLogin> optUserLogin = userLoginRepository.findByEmailAndEntityType(socialLoginDto.getEmail().toLowerCase(),
				Role.CUSTOMER.getStatusValue());
		if (optUserLogin.isPresent()) {
			Optional<Customer> optCustomer = customerRepository.findByEmail(socialLoginDto.getEmail().toLowerCase());
			if (!optCustomer.isPresent()) {
				throw new NotFoundException(messageByLocaleService.getMessage("customer.not.found.email", new Object[] { socialLoginDto.getEmail() }));
			}
			if ((RegisterVia.GOOGLE.getStatusValue().equals(socialLoginDto.getRegisteredVia())
					&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(optUserLogin.get().getGoogleKey()))
					|| (RegisterVia.FACEBOOK.getStatusValue().equals(socialLoginDto.getRegisteredVia())
							&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(optUserLogin.get().getFacebookKey()))) {
				userLoginDto.setUserId(optUserLogin.get().getId());
				return userLoginDto;
			} else {
				if (!optUserLogin.get().getActive().booleanValue()) {
					optUserLogin.get().setActive(Boolean.TRUE);
					userLoginDto.setNewCustomer(true);
				}
				/**
				 * User is present but it is login with another social then set key accordingly
				 */
				if ("GOOGLE".equals(socialLoginDto.getRegisteredVia())) {
					optUserLogin.get().setGoogleKey(CommonUtility.generateBcrypt(socialLoginDto.getUniqueId()));

				} else {
					optUserLogin.get().setFacebookKey(CommonUtility.generateBcrypt(socialLoginDto.getUniqueId()));
				}
				optCustomer.get().setEmailVerified(true);
				optCustomer.get().setStatus(CustomerStatus.ACTIVE.getStatusValue());
				userLoginRepository.save(optUserLogin.get());
				customerRepository.save(optCustomer.get());
				userLoginDto.setUserId(optUserLogin.get().getId());
				return userLoginDto;
			}
		} else {
			/**
			 * User is not exists hence create new customer & user login
			 */
			final CustomerDTO customerDto = new CustomerDTO();
			BeanUtils.copyProperties(socialLoginDto, customerDto);
			customerDto.setPassword(socialLoginDto.getUniqueId());
			customerDto.setActive(true);
			final Long userId = customerService.addCustomer(customerDto, true);
			userLoginDto.setUserId(userId);
			userLoginDto.setNewCustomer(true);
			return userLoginDto;
		}

	}

	@Override
	public UserLogin getUserLoginBasedOnEntityIdAndEntityType(final Long entityId, final String entityType) throws ValidationException {
		if (entityId != null && entityType != null) {
			return userLoginRepository.findByEntityIdAndEntityType(entityId, entityType).orElseThrow(
					() -> new ValidationException(messageByLocaleService.getMessage("user.login.not.found", new Object[] { entityId, entityType })));
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("entity.not.null", null));
		}
	}

	@Override
	public void verifyUser(final Long userId, final String otp) throws ValidationException, NotFoundException {
		/**
		 * Here the logic for userOtp verification.</br>
		 * Here we have check only for Email.
		 */
		if (otpService.verifyOtp(userId, UserOtpTypeEnum.EMAIL.name(), otp)) {
			final UserLogin userLogin = getUserLoginDetail(userId);
			userLogin.setActive(true);
			if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
				vendorService.verifyEmail(userLogin.getEntityId());
			} else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
				customerService.verifyEmail(userLogin.getEntityId());
			} else if (UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
				deliveryBoyService.verifyEmail(userLogin.getEntityId());
			} else {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_USER_TYPE, null));
			}
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("user.otp.not.verified", null));
		}

	}

	@Override
	public UserLogin updatePassword(final PasswordDTO passwordDTO) throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		Boolean canChangePassword = !(userLogin.getPassword() == null
				&& (userLogin.getFacebookKey() != null || userLogin.getGoogleKey() != null || userLogin.getOtp() != null));
		if (Boolean.TRUE.equals(canChangePassword)) {
			if (passwordDTO.getOldPassword() == null) {
				throw new ValidationException(messageByLocaleService.getMessage("old.password.not.null", null));
			}
			changePassword(passwordDTO, userLogin.getId(), userLogin);
		} else {
			userLogin.setPassword(CommonUtility.generateBcrypt(passwordDTO.getNewPassword()));
			userLogin.setUpdatedBy(userLogin.getId());
			userLoginRepository.save(userLogin);
		}
		return userLogin;
	}

	public void changePassword(final PasswordDTO passwordDTO, final Long userId, final UserLogin userLogin) throws ValidationException {
		if (BCrypt.checkpw(passwordDTO.getOldPassword(), userLogin.getPassword())) {
			userLogin.setPassword(CommonUtility.generateBcrypt(passwordDTO.getNewPassword()));
			userLogin.setUpdatedBy(userId);
			userLoginRepository.save(userLogin);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("old.password.not.match", null));
		}
	}

	@Override
	public void sendWelComeEmail(final Long userId) throws NotFoundException {
		final Notification notification = new Notification();
		notification.setCustomerId(getUserLoginDetail(userId).getEntityId());
		notification.setType(NotificationQueueConstants.CUSTOMER_REGISTRATION);
		jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
	}

	@Override
	public void updateEmailForAdmin(final String email) throws ValidationException {
		/**
		 * if admin panel's users(other then super_admin) contains this email then throw validation
		 */
		if (userLoginRepository.findByEmailAndEntityTypeIn(email.toLowerCase(), UserType.ADMIN_PANEL_USER_LIST).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("email.not.unique", null));
		} else {
			UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
			if ("SUPER_ADMIN".equals(userLogin.getRole())) {
				userLogin.setEmail(email.toLowerCase());
				userLoginRepository.save(userLogin);
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("email.can.not.update", null));
			}
		}
	}

	@Override
	public LoginResponse checkUserLogin(final UserLoginDto userLoginDto) throws ValidationException, NotFoundException, UnAuthorizationException {
		String url = ServletUriComponentsBuilder.fromCurrentContextPath().path("/").toUriString();
		LoginResponse loginResponse = generateAuthToken(url, userLoginDto);
		/**
		 * Invalidate the OTP once token generated
		 */
		if (RegisterVia.OTP.getStatusValue().equals(userLoginDto.getRegisteredVia())) {
			final UserLogin userLogin = userLoginRepository.findByPhoneNumberIgnoreCaseAndEntityType(userLoginDto.getUserName(), Role.CUSTOMER.getStatusValue())
					.orElseThrow(() -> new NotFoundException(
							messageByLocaleService.getMessage(CUSTOMER_NOT_FOUND_PHONE, new Object[] { userLoginDto.getUserName() })));
			/**
			 * Generate OTP and save OTP in userLoing table
			 */
			String otp = String.valueOf(CommonUtility.getRandomNumber());
			userLogin.setOtp(CommonUtility.generateBcrypt(otp));
			userLoginRepository.save(userLogin);
		}
		return loginResponse;
	}

	private LoginResponse generateAuthToken(final String url, final UserLoginDto userLoginDto) throws UnAuthorizationException {

		RestTemplate restTemplate = null;
		LoginResponse result = null;
		MultiValueMap<String, String> map = null;
		HttpHeaders headers = null;

		ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

		restTemplate = new RestTemplate(requestFactory);

		String plainCreds = Constant.CLIENT_ID + ":" + Constant.SECRET_ID;
		byte[] plainCredsBytes = plainCreds.getBytes();
		byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
		String base64Creds = new String(base64CredsBytes);

		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		headers.add("Accept", MediaType.APPLICATION_JSON_VALUE);
		headers.add("Authorization", "Basic " + base64Creds);

		map = new LinkedMultiValueMap<>();
		map.add("grant_type", Constant.GRANT_TYPE);
		map.add("username", userLoginDto.getUserName().concat("!!").concat(userLoginDto.getUserType()).concat("#").concat(userLoginDto.getRegisteredVia()));
		map.add("password", userLoginDto.getPassword());

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);
		String outhURL = url + "oauth/token";
		ResponseEntity<LoginResponse> response = restTemplate.postForEntity(outhURL, request, LoginResponse.class);
		result = response.getBody();
		if (result.getStatus() == (HttpStatus.UNAUTHORIZED.value())) {
			throw new UnAuthorizationException(result.getMessage());
		}

		return getUserInfo(result, userLoginDto);
	}

	@Override
	public void forgotPassword(final ForgotPasswordParameterDTO forgotPasswordParameterDTO) throws ValidationException, NotFoundException, MessagingException {
		/**
		 * verify type and if type is email then email is required and if type is sms then phone number is required
		 */
		if (!(forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name())
				|| forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.SMS.name()))) {
			throw new ValidationException(messageByLocaleService.getMessage("otp.type.required", null));
		} else if ((forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name())
				&& !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(forgotPasswordParameterDTO.getEmail()))
				|| (forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.SMS.name())
						&& !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(forgotPasswordParameterDTO.getPhoneNumber()))) {
			throw new ValidationException(messageByLocaleService
					.getMessage(forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name()) ? "email.not.null" : "phone.number.not.null", null));
		} else {
			String userName;
			if (forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name())) {
				userName = forgotPasswordParameterDTO.getEmail().toLowerCase();
			} else {
				userName = forgotPasswordParameterDTO.getPhoneNumber().toLowerCase();
			}
			Optional<UserLogin> userLogin = getUserLoginBasedOnUserNameAndUserType(userName, forgotPasswordParameterDTO.getUserType());
			if (userLogin.isPresent()) {
				/**
				 * generate OTP
				 */
				final UserOtpDto userOtpDto = new UserOtpDto();
				userOtpDto.setUserLoginId(userLogin.get().getId());
				userOtpDto.setType(forgotPasswordParameterDTO.getType());
				final UserOtp userOtp = otpService.generateOtp(userOtpDto);
				if (forgotPasswordParameterDTO.getType().equals(UserOtpTypeEnum.EMAIL.name())) {
					sendForgotPasswordLink(userOtp.getOtp(), userLogin.get().getEmail(), forgotPasswordParameterDTO.getUserType(),
							forgotPasswordParameterDTO.getSendingType());
				} else {
					userOtpDto.setPhoneNumber(forgotPasswordParameterDTO.getPhoneNumber().toLowerCase());
					otpService.sendOtp(userOtpDto, userOtp.getUserLogin(), userOtp.getOtp());
				}
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("user.not.found.username",
						new Object[] { CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(forgotPasswordParameterDTO.getEmail())
								? forgotPasswordParameterDTO.getEmail()
								: forgotPasswordParameterDTO.getPhoneNumber() }));
			}
		}
	}

	@Override
	public Optional<UserLogin> getUserLoginBasedOnUserNameAndUserType(final String userName, final String userType) throws ValidationException {

		/**
		 * when user type is user then check is email or phone is exist for super admin or any admin panel users
		 */
		if (Constant.USER.equalsIgnoreCase(userType)) {
			return userLoginRepository.getAdminPanelUserBasedOnUserNameAndEntityType(userName, UserType.ADMIN_PANEL_USER_LIST);
		} else if (Constant.CUSTOMER.equalsIgnoreCase(userType) || Constant.DELIVERY_BOY.equalsIgnoreCase(userType)) {
			return userLoginRepository.findByEmailAndEntityTypeOrPhoneNumberIgnoreCaseAndEntityType(userName,
					Constant.CUSTOMER.equalsIgnoreCase(userType) ? UserType.CUSTOMER.name() : UserType.DELIVERY_BOY.name(), userName,
					Constant.CUSTOMER.equalsIgnoreCase(userType) ? UserType.CUSTOMER.name() : UserType.DELIVERY_BOY.name());
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_USER_TYPE, null));
		}
	}

	@Override
	public String resetPassword(final ResetPasswordParameterDTO resetPasswordParameterDTO) throws ValidationException, NotFoundException {
		if (!(Constant.CUSTOMER.equalsIgnoreCase(resetPasswordParameterDTO.getUserType())
				|| Constant.USER.equalsIgnoreCase(resetPasswordParameterDTO.getUserType())
				|| Constant.DELIVERY_BOY.equalsIgnoreCase(resetPasswordParameterDTO.getUserType()))) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_USER_TYPE, null));
		} else if (otpService.verifyOtp(resetPasswordParameterDTO.getEmail().toLowerCase(), resetPasswordParameterDTO.getType(),
				resetPasswordParameterDTO.getOtp(), resetPasswordParameterDTO.getUserType())) {
			final Optional<UserLogin> userLogin = getUserLoginBasedOnUserNameAndUserType(resetPasswordParameterDTO.getEmail().toLowerCase(),
					resetPasswordParameterDTO.getUserType());
			if (userLogin.isPresent()) {
				userLogin.get().setPassword(CommonUtility.generateBcrypt(resetPasswordParameterDTO.getPassword()));
				userLoginRepository.save(userLogin.get());
				return messageByLocaleService.getMessage("reset.password.successful", null);
			} else {
				throw new ValidationException(
						messageByLocaleService.getMessage("user.not.found.username", new Object[] { resetPasswordParameterDTO.getEmail() }));
			}
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("reset.password.unsuccessful", null));
		}
	}

	@Override
	public String generateOtpForLogin(final String phoneNumber) throws ValidationException, NotFoundException {
		/**
		 * First check whether user(customer) exist or not Here userName : PhoneNumber and password : OTP
		 */
		final Optional<UserLogin> optUserLogin = userLoginRepository.findByPhoneNumberIgnoreCaseAndEntityType(phoneNumber, Role.CUSTOMER.getStatusValue());
		if (optUserLogin.isPresent()) {
			/**
			 * Additional check whether customer is available or not.
			 */
			Optional<Customer> optCustomer = customerRepository.findByPhoneNumberIgnoreCase(phoneNumber);
			if (!optCustomer.isPresent()) {
				throw new NotFoundException(messageByLocaleService.getMessage(CUSTOMER_NOT_FOUND_PHONE, new Object[] { phoneNumber }));
			}

			/**
			 * Generate OTP and save OTP in userLoing table
			 */
			String otp = String.valueOf(CommonUtility.getRandomNumber());
			optUserLogin.get().setOtp(CommonUtility.generateBcrypt(otp));
			userLoginRepository.save(optUserLogin.get());
			return otp;
		} else {
			/**
			 * Generate OTP and save OTP as password because it is internally save in userLogin table
			 */
			String otp = String.valueOf(CommonUtility.getRandomNumber());

			/**
			 * User is not exists hence create new customer & user login
			 */
			final CustomerDTO customerDto = new CustomerDTO();
			customerDto.setRegisteredVia(RegisterVia.OTP.getStatusValue());
			customerDto.setPhoneNumber(phoneNumber.toLowerCase());
			customerDto.setPassword(otp);
			customerDto.setActive(true);
			customerService.addCustomer(customerDto, false);
			return otp;
		}
	}

	private LoginResponse getUserInfo(final LoginResponse loginResponse, final UserLoginDto userLoginDto) {
		Optional<UserLogin> userLogin;
		if (RegisterVia.OTP.getStatusValue().equals(userLoginDto.getRegisteredVia())) {
			userLogin = userLoginRepository.findByPhoneNumberIgnoreCaseAndEntityType(userLoginDto.getUserName().toLowerCase(), userLoginDto.getUserType());
		} else {
			userLogin = userLoginRepository.findByEmailAndEntityType(userLoginDto.getUserName().toLowerCase(), userLoginDto.getUserType());
		}
		if (userLogin.isPresent()) {
			BeanUtils.copyProperties(userLogin.get(), loginResponse);
			loginResponse.setUserId(userLogin.get().getId());
			if (Role.CUSTOMER.getStatusValue().equals(userLogin.get().getRole())) {
				loginResponse.setCanChangePassword(!(userLogin.get().getPassword() == null
						&& (userLogin.get().getFacebookKey() != null || userLogin.get().getGoogleKey() != null || userLogin.get().getOtp() != null)));
			}
		} else {
			userLogin = userLoginRepository.findByEmailAndRole(userLoginDto.getUserName().toLowerCase(), Role.SUPER_ADMIN.getStatusValue());
			if (userLogin.isPresent()) {
				BeanUtils.copyProperties(userLogin.get(), loginResponse);
				loginResponse.setUserId(userLogin.get().getId());
			}
		}
		return loginResponse;
	}

	@Override
	public Optional<UserLogin> getUserLoginBasedOnPhoneNumberAndEntityType(final String phoneNumber, final String entityType) {
		return userLoginRepository.findByPhoneNumberIgnoreCaseAndEntityType(phoneNumber, entityType);
	}

	@Override
	public void checkOtpForLogin(final UserLoginDto userLoginDto) throws ValidationException, NotFoundException {
		final Optional<UserLogin> optUserLogin = userLoginRepository.findByPhoneNumberIgnoreCaseAndEntityType(userLoginDto.getUserName(),
				Role.CUSTOMER.getStatusValue());
		if (optUserLogin.isPresent() && BCrypt.checkpw(userLoginDto.getPassword(), optUserLogin.get().getOtp())) {
			/**
			 * OTP Verified. Check userLogin is active or not . if not then activate customer and activate userLogin
			 */
			if (!optUserLogin.get().getActive().booleanValue()) {
				UserLogin userLogin = optUserLogin.get();
				userLogin.setActive(true);
				userLoginRepository.save(userLogin);
				Customer customer = customerRepository.findByPhoneNumberIgnoreCase(userLoginDto.getUserName()).orElseThrow(
						() -> new NotFoundException(messageByLocaleService.getMessage(CUSTOMER_NOT_FOUND_PHONE, new Object[] { userLoginDto.getUserName() })));
				customer.setMobileVerified(true);
				customer.setActive(true);
				customer.setStatus(CustomerStatus.ACTIVE.getStatusValue());
				customerRepository.save(customer);
			}
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.expire.otp", null));
		}
	}

	@Override
	public Optional<UserLogin> getUserLoginBasedOnEmailAndRole(final String email, final String role) {
		return userLoginRepository.findByEmailAndRole(email.toLowerCase(), role);
	}

	@Override
	public LoginResponse adminLogin(final UserLoginDto userLoginDto) throws ValidationException, NotFoundException, UnAuthorizationException {
		Optional<UserLogin> optUserLogin = userLoginRepository.getAdminPanelUserBasedOnUserNameAndEntityType(userLoginDto.getUserName().toLowerCase(),
				UserType.ADMIN_PANEL_USER_LIST);
		if (optUserLogin.isPresent()) {
			String entityType = optUserLogin.get().getEntityType() == null ? UserType.USER.name() : optUserLogin.get().getEntityType();
			userLoginDto.setUserType(entityType);
			userLoginDto.setRegisteredVia(RegisterVia.APP.getStatusValue());
			return checkUserLogin(userLoginDto);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.username", null));
		}
	}

	@Override
	public void checkPasswordForUser(final Long entityId, final String entityType, final String password) throws ValidationException {
		UserLogin userLogin = getUserLoginBasedOnEntityIdAndEntityType(entityId, entityType);
		if (!BCrypt.checkpw(password, userLogin.getPassword())) {
			throw new ValidationException(messageByLocaleService.getMessage("password.match.failed", null));
		}
	}

	@Override
	public String addUpdateEmail(final EmailUpdateDTO emailUpdateDTO) throws NotFoundException, ValidationException {
		String userName = null;
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		if (otpService.verifyOtp(userLogin.getId(), UserOtpTypeEnum.EMAIL.name(), emailUpdateDTO.getOtp())) {

			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType())
					&& !userLogin.getEntityType().equalsIgnoreCase(emailUpdateDTO.getUserType())) {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_USER_TYPE, null));
			}
			/**
			 * if any other user has same email then throw exception
			 */
			if (UserType.CUSTOMER.name().equalsIgnoreCase(emailUpdateDTO.getUserType())
					&& customerRepository.findByEmailAndIdNot(emailUpdateDTO.getEmail().toLowerCase(), userLogin.getEntityId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("customer.email.exists", null));
			} else if (UserType.VENDOR.name().equalsIgnoreCase(emailUpdateDTO.getUserType())
					|| UserType.USER.name().equalsIgnoreCase(emailUpdateDTO.getUserType())) {
				Optional<UserLogin> optUserLogin = getUserLoginBasedOnUserNameAndUserType(emailUpdateDTO.getEmail().toLowerCase(),
						emailUpdateDTO.getUserType());
				if (optUserLogin.isPresent() && !optUserLogin.get().getId().equals(userLogin.getId())) {
					throw new ValidationException(messageByLocaleService.getMessage("user.email.exists", new Object[] { emailUpdateDTO.getEmail() }));
				}
			} else if (UserType.DELIVERY_BOY.name().equalsIgnoreCase(emailUpdateDTO.getUserType())
					&& deliveryBoyRepository.findByEmailAndIdNot(emailUpdateDTO.getEmail().toLowerCase(), userLogin.getEntityId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("deliveryBoy.email.not.unique", null));
			}
			/**
			 * when customer add's an email for first time then password is required
			 */
			if (UserType.CUSTOMER.name().equalsIgnoreCase(emailUpdateDTO.getUserType())
					&& !CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getPassword())) {
				throw new ValidationException(messageByLocaleService.getMessage("password.not.null", null));
			}

			userName = updateUserDetail(emailUpdateDTO, userName, userLogin);
			return userName;
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("user.otp.not.verified", null));
		}
	}

	/**
	 * @param  emailUpdateDTO
	 * @param  userName
	 * @param  userLogin
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private String updateUserDetail(final EmailUpdateDTO emailUpdateDTO, String userName, final UserLogin userLogin)
			throws NotFoundException, ValidationException {
		/**
		 * if email is not null it means there is possibility that user is logged in with old email right now
		 */
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEmail())) {
			/**
			 * revoke token of this user name if exist
			 */
			userName = userLogin.getEmail();
		}
		/**
		 * set email and password in user login
		 */
		userLogin.setEmail(emailUpdateDTO.getEmail().toLowerCase());
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(emailUpdateDTO.getPassword())) {
			userLogin.setPassword(CommonUtility.generateBcrypt(emailUpdateDTO.getPassword()));
		}
		updateUserLogin(userLogin);

		if (UserType.CUSTOMER.name().equalsIgnoreCase(emailUpdateDTO.getUserType())) {
			Customer customer = customerService.getCustomerDetails(userLogin.getEntityId());
			customer.setEmailVerified(true);
			customer.setEmail(emailUpdateDTO.getEmail().toLowerCase());
			customerRepository.save(customer);
		} else if (UserType.DELIVERY_BOY.name().equalsIgnoreCase(emailUpdateDTO.getUserType())) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(userLogin.getEntityId());
			DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = deliveryBoyService.getDeliveryBoyCurrentStatusDetail(deliveryBoy);
			/**
			 * if delivery boy is out for delivery then he can not update his email
			 */
			if (deliveryBoyCurrentStatus.getIsBusy().booleanValue()) {
				throw new ValidationException(messageByLocaleService.getMessage("assigned.order.exist", null));
			}
			deliveryBoy.setIsEmailVerified(true);
			deliveryBoy.setEmail(emailUpdateDTO.getEmail().toLowerCase());
			deliveryBoyRepository.save(deliveryBoy);
		} else if (UserType.VENDOR.name().equalsIgnoreCase(emailUpdateDTO.getUserType())) {
			Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
			if (!VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
				throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
			}
			vendor.setEmail(emailUpdateDTO.getEmail().toLowerCase());
			vendor.setIsEmailVerified(true);
			vendorRepository.save(vendor);
		} else if (UserType.USER.name().equalsIgnoreCase(emailUpdateDTO.getUserType()) && userLogin.getEntityId() != null) {
			Users users = usersService.getUsersDetails(userLogin.getEntityId());
			users.setEmail(emailUpdateDTO.getEmail().toLowerCase());
			usersRepository.save(users);
		}
		return userName;
	}

	@Override
	public String addUpdatePhoneNumber(final String phoneNumber, final String otp, final String userType) throws NotFoundException, ValidationException {
		String userName = null;
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		if (otpService.verifyOtp(userLogin.getId(), UserOtpTypeEnum.SMS.name(), otp)) {

			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && !userLogin.getEntityType().equalsIgnoreCase(userType)) {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_USER_TYPE, null));
			}
			/**
			 * if any other user has same phone number then throw exception
			 */
			if (UserType.CUSTOMER.name().equalsIgnoreCase(userType)
					&& customerRepository.findByPhoneNumberIgnoreCaseAndIdNot(phoneNumber, userLogin.getEntityId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("customer.phone.exists", null));
			} else if (UserType.VENDOR.name().equalsIgnoreCase(userType)
					&& vendorRepository.findByContactNoAndIdNot(phoneNumber, userLogin.getEntityId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.contact.not.unique", null));
			} else if (UserType.DELIVERY_BOY.name().equalsIgnoreCase(userType)
					&& deliveryBoyRepository.findByPhoneNumberIgnoreCaseAndIdNot(phoneNumber, userLogin.getEntityId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.phone.exists", null));
			}

			return updateUserDetail(phoneNumber, otp, userType, userName, userLogin);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("user.otp.not.verified", null));
		}
	}

	/**
	 * @param phoneNumber
	 * @param otp
	 * @param userType
	 * @param userName
	 * @param userLogin
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private String updateUserDetail(final String phoneNumber, final String otp, final String userType, String userName, final UserLogin userLogin)
			throws NotFoundException, ValidationException {
		/**
		 * if phone number is not null it means there is possibility that customer is
		 * logged in with old phone number right now
		 */
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getPhoneNumber())) {
			/**
			 * revoke token of this user name if exist
			 */
			userName = userLogin.getPhoneNumber();
		}

		if (UserType.CUSTOMER.name().equalsIgnoreCase(userType)) {
			/**
			 * set phone number and otp in user login of this customer
			 */
			userLogin.setPhoneNumber(phoneNumber);
			userLogin.setOtp(CommonUtility.generateBcrypt(otp));
			updateUserLogin(userLogin);

			Customer customer = customerService.getCustomerDetails(userLogin.getEntityId());
			customer.setMobileVerified(true);
			customer.setPhoneNumber(phoneNumber);
			customerRepository.save(customer);
		} else if (UserType.DELIVERY_BOY.name().equalsIgnoreCase(userType)) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(userLogin.getEntityId());
			deliveryBoy.setIsPhoneNumberVerified(true);
			deliveryBoy.setPhoneNumber(phoneNumber);
			deliveryBoyRepository.save(deliveryBoy);
		} else if (UserType.VENDOR.name().equalsIgnoreCase(userType)) {
			Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
			if (!VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
				throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
			}
			vendor.setIsContactVerified(true);
			vendor.setContactNo(phoneNumber);
			vendorRepository.save(vendor);
		}
		return userName;
	}

}
