package com.nice.service;

import java.util.Optional;

import javax.mail.MessagingException;

import com.nice.dto.LoginResponse;
import com.nice.dto.PasswordDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UpdatePasswordParameterDTO;
import com.nice.dto.UserLoginDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public interface UserLoginService {

	/**
	 * Add user login
	 *
	 * @param  userLogin
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin addUserLogin(UserLogin userLogin) throws NotFoundException;

	/**
	 * Update user login
	 *
	 * @param  userLogin
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin updateUserLogin(UserLogin userLogin) throws NotFoundException;

	/**
	 * @param  userLoginId
	 * @return
	 */
	Optional<UserLogin> getUserLogin(Long userLoginId);

	/**
	 * get user login by id
	 *
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin getUserLoginDetail(Long userId) throws NotFoundException;

	/**
	 * Social login using Facebook and Google
	 *
	 * @param  socialLoginDto
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	UserLoginDto socialLogin(SocialLoginDto socialLoginDto) throws ValidationException, NotFoundException;

	/**
	 * Get user login based on entityId and entityType
	 *
	 * @param  entityId
	 * @param  entityType
	 * @return
	 * @throws ValidationException
	 */
	UserLogin getUserLoginBasedOnEntityIdAndEntityType(Long entityId, String entityType) throws ValidationException;

	/**
	 * Verify user using email
	 *
	 * @param  userId
	 * @param  otp
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void verifyUser(Long userId, String otp) throws ValidationException, NotFoundException;

	/**
	 * Update password based on login user and old password
	 *
	 * @param  passwordDTO
	 * @param  userId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	UserLogin updatePassword(PasswordDTO passwordDTO) throws ValidationException, NotFoundException;

	/**
	 * Send welcome email
	 *
	 * @param  userId
	 * @throws NotFoundException
	 */
	void sendWelComeEmail(Long userId) throws NotFoundException;

	/**
	 * update email of admin (This method is used for admin to update his/her email from setting menu)
	 *
	 * @param  email
	 * @param  userId
	 * @throws ValidationException
	 */
	void updateEmailForAdmin(String email) throws ValidationException;

	/**
	 * @param  userLoginDto
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	LoginResponse checkUserLogin(UserLoginDto userLoginDto) throws ValidationException, NotFoundException, UnAuthorizationException;

	/**
	 * get user login based on email and entityType
	 *
	 * @param  email
	 * @param  entityType
	 * @return
	 */
	Optional<UserLogin> getUserLoginBasedOnEmailAndEntityType(String email, String entityType);

	/**
	 * generate Link or OTP for user on forgot password
	 *
	 * @param  updatePasswordParameterDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	void forgotPassword(UpdatePasswordParameterDTO updatePasswordParameterDTO) throws ValidationException, NotFoundException, MessagingException;

	/**
	 * reset password from forgot password
	 *
	 * @param  email
	 * @param  otp
	 * @param  password
	 * @param  type
	 * @param  userType
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String resetPassword(String email, String otp, String password, String type, String userType) throws ValidationException, NotFoundException;

	/**
	 * get user login based on userName and userType
	 *
	 * @param  userName
	 * @param  userType
	 * @return
	 * @throws ValidationException
	 */
	Optional<UserLogin> getUserLoginBasedOnUserNameAndUserType(String userName, String userType) throws ValidationException;

	/**
	 * @param  phoneNumber
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String generateOtpForLogin(String phoneNumber) throws ValidationException, NotFoundException;

	/**
	 * Check OTP. if OTP is correct then activate customer.
	 *
	 * @param  userLoginDto
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void checkOtpForLogin(UserLoginDto userLoginDto) throws ValidationException, NotFoundException;

	/**
	 * @param  email
	 * @param  name
	 * @return
	 */
	Optional<UserLogin> getUserLoginBasedOnPhoneNumberAndEntityType(String email, String name);

	/**
	 * get user login based on email and role
	 *
	 * @param  email
	 * @param  name
	 * @return
	 */
	Optional<UserLogin> getUserLoginBasedOnEmailAndRole(String email, String role);

}
