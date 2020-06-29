package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.mail.MessagingException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCrypt;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.CustomerStatus;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.RegisterVia;
import com.nice.constant.Role;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.CustomerDTO;
import com.nice.dto.Notification;
import com.nice.dto.PasswordDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UserInfo;
import com.nice.dto.UserOtpDto;
import com.nice.exception.BaseRuntimeException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Customer;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.repository.CustomerRepository;
import com.nice.repository.UserLoginRepository;
import com.nice.service.CustomerService;
import com.nice.service.OtpService;
import com.nice.service.UserLoginService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Service(value = "userLoginService")
@Transactional(rollbackFor = Throwable.class)
public class UserLoginServiceImpl implements UserLoginService, UserDetailsService {

	/**
	 *
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UserLoginServiceImpl.class);
	private static final String USER_NOT_EXISTS_EMAIL = "user.not.exists.email";
	private static final String USER_TYPE = "userType";

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

		/**
		 * Fetch user based on email(userName)
		 */
		Optional<UserLogin> optUserLogin = userLoginRepository.findByEmailAndEntityType(actualUser, userType);
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
				// try {
				// customer = customerService.getCustomerDetails(optUserLogin.get().getEntityId());
				// } catch (final NotFoundException e) {
				// LOGGER.error("Customer not found for customer Id: {} ", optUserLogin.get().getEntityId());
				// }
				/**
				 * If user status is deactivate: then send related message
				 */
				if ((customer != null) && customer.getStatus().equals(CustomerStatus.DE_ACTIVE.getStatusValue())) {
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage("user.account.unauthorized.admin", null));
				} else {
					/**
					 * If user is not activated yet then send related message
					 */
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED,
							messageByLocaleService.getMessage("user.email.not.activate", new Object[] { actualUser }));
				}
			} else {
				throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage("user.account.unauthorized.admin", null));
			}

		}

		final String role = optUserLogin.get().getRole();
		final SimpleGrantedAuthority authority = new SimpleGrantedAuthority("ROLE_" + role);
		if (RegisterVia.GOOGLE.getStatusValue().equals(requestVia)) {
			return new UserAwareUserDetails(actualUser, optUserLogin.get().getGoogleKey(), Arrays.asList(authority), optUserLogin.get());
		} else if (RegisterVia.FACEBOOK.getStatusValue().equals(requestVia)) {
			return new UserAwareUserDetails(actualUser, optUserLogin.get().getFacebookKey(), Arrays.asList(authority), optUserLogin.get());
		} else {
			if (optUserLogin.get().getPassword() == null) {
				throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, messageByLocaleService.getMessage("user.unauthorized.social", null));
			}
			return new UserAwareUserDetails(actualUser, optUserLogin.get().getPassword(), Arrays.asList(authority), optUserLogin.get());
		}
	}

	@Override
	public UserLogin addUserLogin(final UserLogin userLogin, final Long userId) throws NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getFacebookKey())) {
			userLogin.setFacebookKey(CommonUtility.generateBcrypt(userLogin.getFacebookKey()));
		} else if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getGoogleKey())) {
			userLogin.setGoogleKey(CommonUtility.generateBcrypt(userLogin.getGoogleKey()));
		} else {
			userLogin.setPassword(CommonUtility.generateBcrypt(userLogin.getPassword()));
		}
		if (userId != null) {
			userLogin.setCreatedBy(userId);
			userLogin.setUpdatedBy(userId);
		}
		return userLoginRepository.save(userLogin);
	}

	@Override
	public UserLogin updateUserLogin(final UserLogin userLogin) throws NotFoundException {
		return userLoginRepository.save(userLogin);
	}

	@Override
	public Optional<UserLogin> getUserLoginBasedOnEmail(final String email) {
		return userLoginRepository.findByEmail(email);
	}

	@Override
	public Optional<UserLogin> getUserLogin(final Long userId) {
		return userLoginRepository.findById(userId);
	}

	@Override
	public UserLogin getUserLoginDetailBasedOnEmail(final String email) throws NotFoundException {
		return userLoginRepository.findByEmail(email)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("user.not.found.email", new Object[] { email })));
	}

	@Override
	public UserLogin getUserLoginDetail(final Long userId) throws NotFoundException {
		return userLoginRepository.findById(userId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userId })));
	}

	@Override
	public void forgotPasswordSendOtpForDeliveryBoy(final String email) throws ValidationException, NotFoundException, MessagingException {
		LOGGER.info("inside delivery boy forgot password");
		UserLogin userLogin = getUserLoginDetailBasedOnEmail(email);
		if (!Role.DELIVERY_BOY.getStatusValue().equalsIgnoreCase(userLogin.getRole())) {
			throw new ValidationException(messageByLocaleService.getMessage(USER_NOT_EXISTS_EMAIL, new Object[] { email }));
		}
		final UserOtpDto userOtpDto = new UserOtpDto();
		userOtpDto.setUserLoginId(userLogin.getId());
		userOtpDto.setType(UserOtpTypeEnum.SMS.name());
		userOtpDto.setEmail(userLogin.getEmail());
		userOtpDto.setPhoneNumber(userLogin.getEmail());
		final UserOtp userOtp = otpService.generateOtp(userOtpDto);

		otpService.sendOtp(userOtpDto, userOtp.getUserLogin(), userOtp.getOtp());

	}

	@Override
	public void forgotPasswordLinkGenerator(final String email, final String userType) throws ValidationException, NotFoundException {
		UserLogin userLogin = getUserLoginDetailBasedOnEmail(email);
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (userType.equalsIgnoreCase(Constant.CUSTOMER)) {
			if (!Constant.CUSTOMER.equalsIgnoreCase(userLogin.getRole())) {
				throw new ValidationException(messageByLocaleService.getMessage(USER_NOT_EXISTS_EMAIL, new Object[] { email }));
			}
			emailParameterMap.put(USER_TYPE, Constant.CUSTOMER);
		} else if (userType.equalsIgnoreCase(Constant.ADMIN)) {

			if (!Constant.getAdminRoles().contains(userLogin.getRole().toUpperCase())) {
				throw new ValidationException(messageByLocaleService.getMessage(USER_NOT_EXISTS_EMAIL, new Object[] { email }));
			}
			emailParameterMap.put(USER_TYPE, Constant.ADMIN);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.user.type", null));
		}

		final UserOtpDto userOtpDto = new UserOtpDto();
		userOtpDto.setUserLoginId(userLogin.getId());
		userOtpDto.setType(UserOtpTypeEnum.EMAIL.name());
		userOtpDto.setEmail(userLogin.getEmail());
		final UserOtp userOtp = otpService.generateOtp(userOtpDto);

		emailParameterMap.put("userId", String.valueOf(userLogin.getId()));
		emailParameterMap.put("otp", userOtp.getOtp());
		final List<String> sendEmailTo = new ArrayList<>();
		sendEmailTo.add(email);

		sendForgotPasswordLink(userOtp.getOtp(), email, emailParameterMap.get(USER_TYPE), userLogin.getId());
	}

	private void sendForgotPasswordLink(final String otp, final String email, final String userType, final Long userId) {
		final Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setEmail(email);
		notification.setUserType(userType);
		notification.setCustomerId(userId);
		notification.setType(NotificationQueueConstants.FORGOT_PASSWORD);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	@Override
	public String resetPassword(final String otp, final String password, final Long userId, final String type) throws ValidationException, NotFoundException {
		if (otpService.verifyOtp(userId, type, otp)) {
			final UserLogin userLogin = getUserLoginDetail(userId);
			userLogin.setPassword(CommonUtility.generateBcrypt(password));
			userLogin.setUpdatedBy(userId);
			userLoginRepository.save(userLogin);
			return messageByLocaleService.getMessage("reset.password.successful", null);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("reset.password.unsuccessful", null));
		}
	}

	@Override
	public SocialLoginDto socialLogin(final SocialLoginDto socialLoginDto) throws ValidationException, NotFoundException {
		socialLoginDto.setClientId(Constant.CLIENT_ID);
		socialLoginDto.setClientSecret(Constant.SECRET_ID);
		final Optional<UserLogin> optUserLogin = userLoginRepository.findByEmail(socialLoginDto.getEmail());
		if (optUserLogin.isPresent()) {
			if (!optUserLogin.get().getEntityType().equals(Role.CUSTOMER.getStatusValue())) {
				throw new ValidationException(messageByLocaleService.getMessage("same.admin.email.exists", null));
			}
			Optional<Customer> optCustomer = customerRepository.findByEmailIgnoreCase(socialLoginDto.getEmail());
			if (!optCustomer.isPresent()) {
				throw new NotFoundException(
						messageByLocaleService.getMessage("customer.not.found.email", new Object[] { Constant.CUSTOMER, socialLoginDto.getEmail() }));
			}
			if ((RegisterVia.GOOGLE.getStatusValue().equals(socialLoginDto.getRegisteredVia())
					&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(optUserLogin.get().getGoogleKey()))
					|| (RegisterVia.FACEBOOK.getStatusValue().equals(socialLoginDto.getRegisteredVia())
							&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(optUserLogin.get().getFacebookKey()))) {
				socialLoginDto.setUserId(optUserLogin.get().getId());
				/**
				 * to handle old data this code is not required for new database
				 */
				if (!optUserLogin.get().getActive().booleanValue()) {
					optUserLogin.get().setActive(Boolean.TRUE);
					optCustomer.get().setEmailVerified(true);
					optCustomer.get().setStatus(CustomerStatus.ACTIVE.getStatusValue());
					userLoginRepository.save(optUserLogin.get());
					customerRepository.save(optCustomer.get());
				}
				return socialLoginDto;
			} else {
				if (!optUserLogin.get().getActive().booleanValue()) {
					optUserLogin.get().setActive(Boolean.TRUE);
					socialLoginDto.setNewCustomer(true);
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
				socialLoginDto.setUserId(optUserLogin.get().getId());
				return socialLoginDto;
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
			socialLoginDto.setUserId(userId);
			socialLoginDto.setNewCustomer(true);
			return socialLoginDto;
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
			customerService.verifyEmail(userLogin.getEntityId());
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("user.otp.not.verified", new Object[] {}));
		}

	}

	@Override
	public UserLogin updatePassword(final PasswordDTO passwordDTO) throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		UserInfo userInfo = getUserInfo(userLogin.getEmail());
		if (userInfo.getCanChangePassword() != null) {
			if (Boolean.TRUE.equals(userInfo.getCanChangePassword())) {
				if (passwordDTO.getOldPassword() == null) {
					throw new ValidationException(messageByLocaleService.getMessage("old.password.not.null", null));
				}
				changePassword(passwordDTO, userLogin.getId(), userLogin);
			} else {
				userLogin.setPassword(CommonUtility.generateBcrypt(passwordDTO.getNewPassword()));
				userLogin.setUpdatedBy(userLogin.getId());
				userLoginRepository.save(userLogin);
			}
		} else {
			changePassword(passwordDTO, userLogin.getId(), userLogin);
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
	public UserInfo getUserInfo(final String username) throws NotFoundException {
		Optional<UserLogin> userLogin = userLoginRepository.findByEmail(username);
		final UserInfo userInfo = new UserInfo();
		if (userLogin.isPresent()) {
			BeanUtils.copyProperties(userLogin.get(), userInfo);
			if (Role.CUSTOMER.getStatusValue().equals(userLogin.get().getRole())) {
				userInfo.setCanChangePassword(
						!(userLogin.get().getPassword() == null && (userLogin.get().getFacebookKey() != null || userLogin.get().getGoogleKey() != null)));
			}
		}
		return userInfo;
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
		if (userLoginRepository.findByEmailAndIdNot(email, 1L).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("email.not.unique", null));
		} else {
			UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
			if ("SUPER_ADMIN".equals(userLogin.getRole())) {
				userLogin.setEmail(email);
				userLoginRepository.save(userLogin);
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("email.can.not.update", null));
			}
		}
	}

}
