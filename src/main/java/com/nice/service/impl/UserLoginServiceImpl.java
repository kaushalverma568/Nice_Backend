package com.nice.service.impl;

import java.util.Arrays;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.CustomerStatus;
import com.nice.constant.RegisterVia;
import com.nice.constant.Role;
import com.nice.constant.UserType;
import com.nice.exception.BaseRuntimeException;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Customer;
import com.nice.model.UserLogin;
import com.nice.repository.UserLoginRepository;
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

	@Autowired
	private UserLoginRepository userLoginRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

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
		if (!optUserLogin.isPresent() && UserType.USERS.name().equalsIgnoreCase(userType)) {
			optUserLogin = userLoginRepository.findByEmailAndRole(actualUser, Role.SUPER_ADMIN.name());
		}

		/**
		 * If user is not exists then throw an error
		 */
		if (!optUserLogin.isPresent()) {
			throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, "Invalid username");
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
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, "Your account is disable by Admin!! Kindly contact administrator ");
				} else {
					/**
					 * If user is not activated yet then send related message
					 */
					throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, "User is not activated for email : " + actualUser);
				}
			} else {
				throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, "Your account is disable by Admin!! Kindly contact administrator ");
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
				throw new BaseRuntimeException(HttpStatus.UNAUTHORIZED, "Kindly reset the password or login via social media");
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

}
