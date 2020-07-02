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
 * @date : 26-Jun-2020
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
			throw new ValidationException(messageByLocaleService.getMessage("otp.id.email.not.null", null));
		}

		/**
		 * Check if userLogin exists
		 */
		if (!userlogin.isPresent()) {
			LOGGER.error("user login is not present : {}", userOtpDto);
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userOtpDto.getUserLoginId() }));
		}
		/**
		 * Check if otp already generated in past for the user with this OTP Type, if
		 * yes update the existing row, if not make a new object and persist it
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
			String otpMessage = "OTP for your Nice application is : ";
			if (userOtpDto.getPhoneNumber() == null || userOtpDto.getPhoneNumber().isEmpty()) {
				throw new ValidationException(messageByLocaleService.getMessage("user.mobile.required", null));
			}
			smsUtil.sendSMS(userOtpDto.getPhoneNumber(), otpMessage + otp);
		} else {
			LOGGER.error("Proper Mode not Specifed to generate OTP: Specified mode is - {}", userOtpDto.getType());
			throw new ValidationException(messageByLocaleService.getMessage("otp.type.required", null));
		}
	}

	@Override
	public boolean verifyOtp(final String email, final String type, final String otp, final String userType) throws ValidationException, NotFoundException {
		LOGGER.info("Inside fetching OTP for email {} with {} and userType {} for otp {}", email, type, userType, otp);
		Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndUserType(email, userType);
		if (userLogin.isPresent()) {
			return verifyOtp(userLogin.get().getId(), type, otp);
		}
		return false;
	}

	@Override
	public boolean verifyOtp(final Long userLoginId, final String type, final String otp) throws ValidationException, NotFoundException {
		LOGGER.info("Inside fetching OTP for userLogin {} with {} for otp {}", userLoginId, type, otp);
		String placeHolder = messageByLocaleService.getMessage("otp.type.link", null);
		if (UserOtpTypeEnum.SMS.name().equals(type)) {
			placeHolder = messageByLocaleService.getMessage("otp.type.otp", null);
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
					 * Check if the otp is generated only before a specified interval, if not return
					 * false
					 */
					if ((System.currentTimeMillis() - updatedAt.getTime()) / 60000 < Constant.OTP_VALIDITY_TIME_IN_MIN) {
						UserOtp userOtp = optionalUserOtp.get();
						userOtp.setActive(false);
						userOtpRepository.save(userOtp);
						return true;
					} else {
						LOGGER.error("{} expired, was generated at {} ", placeHolder, updatedAt);
						throw new ValidationException(messageByLocaleService.getMessage("otp.expired.generate.new", new Object[] { placeHolder, placeHolder }));
					}
				} else {
					LOGGER.error("{} is already used before otp:{}", placeHolder, otp);
					throw new ValidationException(messageByLocaleService.getMessage("otp.already.used", new Object[] { placeHolder }));
				}
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("otp.incorrect", new Object[] { placeHolder, placeHolder }));
			}
		} else {
			LOGGER.error("No record obtained for userLogin {}", userLoginId);
			throw new ValidationException(messageByLocaleService.getMessage("otp.not.generate", new Object[] { placeHolder }));
		}
	}
}
