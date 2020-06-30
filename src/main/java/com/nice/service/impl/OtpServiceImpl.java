package com.nice.service.impl;

import java.util.Date;
import java.util.Optional;

import javax.mail.MessagingException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.Constant;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.dto.Notification;
import com.nice.dto.UserOtpDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.UserOtpMapper;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.repository.UserOtpRepository;
import com.nice.service.OtpService;
import com.nice.service.UserLoginService;
import com.nice.util.CommonUtility;
import com.nice.util.SMSUtil;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service(value = "userOtpService")
@Transactional(rollbackFor = Throwable.class)
public class OtpServiceImpl implements OtpService {

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private UserOtpRepository userOtpRepository;
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UserOtpMapper userOtpMapper;

	@Autowired
	private SMSUtil smsUtil;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	private static final Logger LOGGER = LoggerFactory.getLogger(OtpServiceImpl.class);

	@Override
	public UserOtp generateOtp(final UserOtpDto userOtpDto) throws NotFoundException, ValidationException {
		LOGGER.info("Inside generating OTP for userDTO : {}", userOtpDto);
		Optional<UserLogin> userlogin = null;
		/**
		 * Check if userId or UserEmail is available to generate OTP.
		 */
		if (userOtpDto.getUserLoginId() != null) {
			userlogin = userLoginService.getUserLogin(userOtpDto.getUserLoginId());
		} else if (userOtpDto.getEmail() != null) {
			userlogin = userLoginService.getUserLoginBasedOnEmail(userOtpDto.getEmail());
		} else {
			LOGGER.error("Neither UserId, not userEmail specified to generate OTP");
			throw new ValidationException("Please specify userId or userEmail to generate the Otp");
		}

		/**
		 * Check if userLogin exists
		 */
		if (!userlogin.isPresent()) {
			LOGGER.error("user login is not present : {}", userOtpDto);
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userOtpDto.getUserLoginId() }));
		}
		/**
		 * Check if otp already generated in past for the user with this OTP Type, if yes update the existing row, if not make a
		 * new object and persist it
		 */
		UserOtp userOtp = userOtpRepository.findByUserLoginAndType(userlogin, userOtpDto.getType());
		if (userOtp == null) {
			userOtp = userOtpMapper.toEntity(userOtpDto, 1L);

		}
		userOtp.setUserLogin(userlogin.get());
		userOtp.setOtp(String.valueOf(CommonUtility.getRandomNumber()));
		userOtp.setActive(true);
		userOtpRepository.save(userOtp);

		LOGGER.info("Generated new Otp : {} for userId : {}", userOtp.getOtp(), userOtp.getId());
		return userOtp;

	}

	@Override
	public void sendOtp(final UserOtpDto userOtpDto, final UserLogin userlogin, final String otp) throws ValidationException, MessagingException {

		if (UserOtpTypeEnum.EMAIL.name().equalsIgnoreCase(userOtpDto.getType())) {

			Notification notification = new Notification();
			notification.setOtp(otp);
			notification.setEmail(userlogin.getEmail());
			notification.setType(NotificationQueueConstants.SEND_OTP);
			jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
		} else if (UserOtpTypeEnum.SMS.name().equalsIgnoreCase(userOtpDto.getType())) {
			String otpMessage = "OTP for your Raasa application is : ";
			if (userOtpDto.getPhoneNumber() == null || userOtpDto.getPhoneNumber().isEmpty()) {
				throw new ValidationException(messageByLocaleService.getMessage("user.mobile.required", null));
			}
			smsUtil.sendSMS(userOtpDto.getPhoneNumber(), otpMessage + otp);
		} else {
			LOGGER.error("Proper Mode not Specifed to generate OTP: Specified mode is - {}", userOtpDto.getType());
			throw new ValidationException("Please specify proper Mode not Specifed to generate OTP: EMAIL or SMS");
		}
	}

	@Override
	public boolean verifyOtp(final String email, final String type, final String otp) throws ValidationException, NotFoundException {
		LOGGER.info("Inside fetching OTP for email {} with {} for otp {}", email, type, otp);
		Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmail(email);
		if (userLogin.isPresent()) {
			return verifyOtp(userLogin.get().getId(), type, otp);
		}
		return false;
	}

	@Override
	public boolean verifyOtp(final Long userLoginId, final String type, final String otp) throws ValidationException, NotFoundException {
		LOGGER.info("Inside fetching OTP for userLogin {} with {} for otp {}", userLoginId, type, otp);
		String placeHolder = "Link";
		if (UserOtpTypeEnum.SMS.name().equals(type)) {
			placeHolder = "OTP";
		}
		Optional<UserLogin> userlogin = userLoginService.getUserLogin(userLoginId);
		if (!userlogin.isPresent()) {
			LOGGER.error("No user present for userLogin {} ", userLoginId);
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userLoginId }));
		}

		Optional<UserOtp> optionalUserOtp = userOtpRepository.findAllByTypeAndUserLogin(type, userlogin.get());

		if (optionalUserOtp.isPresent()) {
			if (optionalUserOtp.get().getOtp().equals(otp)) {
				if (optionalUserOtp.get().getActive().booleanValue()) {
					Date updatedAt = optionalUserOtp.get().getUpdatedAt();
					/**
					 * Check if the otp is generated only before a specified interval, if not return false
					 */
					if ((System.currentTimeMillis() - updatedAt.getTime()) / 60000 < Constant.OTP_VALIDITY_TIME_IN_MIN) {
						UserOtp userOtp = optionalUserOtp.get();
						userOtp.setActive(false);
						userOtpRepository.save(userOtp);
						return true;
					} else {
						LOGGER.error("{} expired, was generated at {} ", placeHolder, updatedAt);
						throw new ValidationException(
								messageByLocaleService.getMessage(placeHolder + " is expired ! Generate a new " + placeHolder + " and try again", null));
					}
				} else {
					LOGGER.error("{} is already used before otp:{}", placeHolder, otp);
					throw new ValidationException(messageByLocaleService.getMessage(placeHolder + " is already used before", null));
				}
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("Incorrect " + placeHolder + " ! Use latest generated " + placeHolder, null));
			}
		} else {
			LOGGER.error("No record obtained for userLogin {}", userLoginId);
			throw new ValidationException(placeHolder + " is not generated for this user");
		}
	}
}
