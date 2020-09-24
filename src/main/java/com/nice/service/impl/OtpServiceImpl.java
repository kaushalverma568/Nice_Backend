package com.nice.service.impl;

import java.util.Date;
import java.util.Optional;

import javax.mail.MessagingException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
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
		 * Check if userId or (UserEmail or phoneNumber) is available to generate OTP.
		 */
		if (userOtpDto.getUserId() != null) {
			userlogin = userLoginService.getUserLogin(userOtpDto.getUserId());
		} else if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userOtpDto.getEmail())
				|| CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userOtpDto.getPhoneNumber())) {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userOtpDto.getUserType())) {
				userlogin = userLoginService.getUserLoginBasedOnUserNameAndUserType(
						CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userOtpDto.getEmail()) ? userOtpDto.getEmail().toLowerCase()
								: userOtpDto.getPhoneNumber(),
						userOtpDto.getUserType());
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("user.type.not.null", null));
			}
		} else {
			LOGGER.error("Neither UserId, not userName specified to generate OTP");
			throw new ValidationException(messageByLocaleService.getMessage("otp.id.email.not.null", null));
		}

		/**
		 * Check if userLogin exists
		 */
		if (!userlogin.isPresent()) {
			LOGGER.error("user login is not present : {}", userOtpDto);
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userOtpDto.getUserId() }));
		}
		/**
		 * Check if otp already generated in past for the user with this OTP Type, if
		 * yes update the existing row, if not make a new object and persist it
		 */
		UserOtp userOtp = userOtpRepository.findByUserLoginAndTypeIgnoreCase(userlogin, userOtpDto.getType());
		if (userOtp == null) {
			userOtp = userOtpMapper.toEntity(userOtpDto, 1L);

		}
		userOtp.setUserLogin(userlogin.get());
		userOtp.setOtp(String.valueOf(CommonUtility.getRandomNumber()));
		userOtp.setActive(true);
		userOtpRepository.save(userOtp);

		LOGGER.info("Generated new Otp : {} for userId : {}", userOtp.getOtp(), userlogin.get().getId());
		return userOtp;

	}

	@Override
	public void sendOtp(final UserOtpDto userOtpDto, final UserLogin userlogin, final String otp) throws ValidationException, MessagingException {

		if (UserOtpTypeEnum.EMAIL.name().equals(userOtpDto.getType())) {

			Notification notification = new Notification();
			notification.setOtp(otp);
			notification
					.setEmail(CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userOtpDto.getEmail()) ? userOtpDto.getEmail().toLowerCase() : userlogin.getEmail());
			notification.setType(NotificationQueueConstants.SEND_OTP);
			notification.setSendingType(userOtpDto.getSendingType());
			notification.setLanguage(LocaleContextHolder.getLocale().getLanguage());
			jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
		} else if (UserOtpTypeEnum.SMS.name().equals(userOtpDto.getType())) {
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
	public void verifyOtp(final String userName, final String type, final String otp, final String userType, final Boolean active)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside fetching OTP for userName {} with {} and userType {} for otp {}", userName, type, userType, otp);
		Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnUserNameAndUserType(userName, userType);
		if (userLogin.isPresent()) {
			verifyOtp(userLogin.get().getId(), type, otp, active);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("user.not.found.username", new Object[] { userName }));
		}
	}

	@Override
	public void verifyOtp(final Long userLoginId, final String type, final String otp, final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside fetching OTP for userLogin {} with {} for otp {}", userLoginId, type, otp);

		Optional<UserLogin> userlogin = userLoginService.getUserLogin(userLoginId);
		if (!userlogin.isPresent()) {
			LOGGER.error("No user present for userLogin {} ", userLoginId);
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { userLoginId }));
		}

		Optional<UserOtp> optionalUserOtp = userOtpRepository.findAllByTypeIgnoreCaseAndUserLogin(type, userlogin.get());

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
						/**
						 * active needs to be FALSE every time except you want same otp to be used for
						 * verification more then one time
						 */
						userOtp.setActive(active);
						userOtpRepository.save(userOtp);
					} else {
						LOGGER.error("otp expired, was generated at {} ", updatedAt);
						throw new ValidationException(messageByLocaleService.getMessage("otp.expired.generate.new", null));
					}
				} else {
					LOGGER.error("otp is already used before otp:{}", otp);
					throw new ValidationException(messageByLocaleService.getMessage("otp.already.used", null));
				}
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("otp.incorrect", null));
			}
		} else {
			LOGGER.error("No record obtained for userLogin {}", userLoginId);
			throw new ValidationException(messageByLocaleService.getMessage("otp.not.generate", null));
		}
	}
}
