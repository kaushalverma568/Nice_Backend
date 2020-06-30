package com.nice.service;

import java.util.Optional;

import javax.mail.MessagingException;

import com.nice.dto.LoginResponse;
import com.nice.dto.PasswordDTO;
import com.nice.dto.SocialLoginDto;
import com.nice.dto.UserInfo;
import com.nice.dto.UserLoginDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
public interface UserLoginService {

	/**
	 * Add user login
	 *
	 * @param userLogin
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin addUserLogin(UserLogin userLogin) throws NotFoundException;

	/**
	 * Update user login
	 *
	 * @param userLogin
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin updateUserLogin(UserLogin userLogin) throws NotFoundException;

	/**
	 * @param email
	 * @return
	 */
	Optional<UserLogin> getUserLoginBasedOnEmail(String email);

	/**
	 * @param userLoginId
	 * @return
	 */
	Optional<UserLogin> getUserLogin(Long userLoginId);

	/**
	 * send otp to delivery boy when he/she forgot password
	 *
	 * @param email
	 * @throws ValidationException
	 * @throws MessagingException
	 * @throws NotFoundException
	 */
	void forgotPasswordSendOtpForDeliveryBoy(String email) throws ValidationException, NotFoundException, MessagingException;

	/**
	 * generate link for redirect through email
	 *
	 * @param email
	 * @param userType
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void forgotPasswordLinkGenerator(String email, String userType) throws ValidationException, NotFoundException;

	/**
	 * reset password from forgot password
	 *
	 * @param otp
	 * @param password
	 * @param userId
	 * @param type
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	String resetPassword(String otp, String password, Long userId, String type) throws ValidationException, NotFoundException;

	/**
	 * get user login by email
	 *
	 * @param email
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin getUserLoginDetailBasedOnEmail(String email) throws NotFoundException;

	/**
	 * get user login by id
	 *
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin getUserLoginDetail(Long userId) throws NotFoundException;

	/**
	 * Social login using Facebook and Google
	 *
	 * @param socialLoginDto
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	SocialLoginDto socialLogin(SocialLoginDto socialLoginDto) throws ValidationException, NotFoundException;

	/**
	 * Get user login based on entityId and entityType
	 *
	 * @param entityId
	 * @param entityType
	 * @return
	 * @throws ValidationException
	 */
	UserLogin getUserLoginBasedOnEntityIdAndEntityType(Long entityId, String entityType) throws ValidationException;

	/**
	 * Verify user using email
	 *
	 * @param userId
	 * @param otp
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void verifyUser(Long userId, String otp) throws ValidationException, NotFoundException;

	/**
	 * Update password based on login user and old password
	 *
	 * @param passwordDTO
	 * @param userId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	UserLogin updatePassword(PasswordDTO passwordDTO) throws ValidationException, NotFoundException;

	/**
	 * get user's basic info
	 *
	 * @param username
	 * @return
	 * @throws NotFoundException
	 */
	UserInfo getUserInfo(String username) throws NotFoundException;

	/**
	 * Send welcome email
	 *
	 * @param userId
	 * @throws NotFoundException
	 */
	void sendWelComeEmail(Long userId) throws NotFoundException;

	/**
	 * update email of admin (This method is used for admin to update his/her email
	 * from setting menu)
	 *
	 * @param email
	 * @param userId
	 * @throws ValidationException
	 */
	void updateEmailForAdmin(String email) throws ValidationException;

	/**
	 * @param userLoginDto
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws UnAuthorizationException
	 */
	LoginResponse checkUserLogin(UserLoginDto userLoginDto) throws ValidationException, NotFoundException, UnAuthorizationException;

	/**
	 * get user login based on email and role
	 *
	 * @param email
	 * @param name
	 * @return
	 */
	Optional<UserLogin> getUserLoginBasedOnEmailAndRole(String email, String role);

}
