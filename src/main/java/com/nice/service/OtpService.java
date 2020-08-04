package com.nice.service;

import java.io.IOException;
import java.security.GeneralSecurityException;

import javax.mail.MessagingException;

import com.nice.dto.UserOtpDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
public interface OtpService {

	/**
	 * @param userOtpDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	UserOtp generateOtp(UserOtpDto userOtpDto) throws NotFoundException, ValidationException;

	/**
	 * Verify Otp using userId
	 *
	 * @param userLoginId
	 * @param type
	 * @param otp
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void verifyOtp(Long userLoginId, String type, String otp, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * Verify Opt using user user name
	 *
	 * @param userName
	 * @param type
	 * @param otp
	 * @param userType
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void verifyOtp(String userName, String type, String otp, String userType, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param userOtpDto
	 * @param userlogin
	 * @param otp
	 * @throws ValidationException
	 * @throws MessagingException
	 * @throws NotFoundException
	 */
	void sendOtp(UserOtpDto userOtpDto, UserLogin userlogin, String otp) throws ValidationException, MessagingException;

}
