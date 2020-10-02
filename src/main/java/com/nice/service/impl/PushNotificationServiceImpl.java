package com.nice.service.impl;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.UserType;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.PushNotification;
import com.nice.model.PushNotificationReceiver;
import com.nice.model.UserLogin;
import com.nice.repository.PushNotificationReceiverRepository;
import com.nice.repository.PushNotificationRepository;
import com.nice.service.PushNotificationService;
import com.nice.service.UserLoginService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
@Service(value = "pushNotificationService")
@Transactional(rollbackFor = Throwable.class)
public class PushNotificationServiceImpl implements PushNotificationService {

	@Autowired
	private PushNotificationRepository pushNotificationRepository;

	@Autowired
	private PushNotificationReceiverRepository pushNotificationReceiverRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UserLoginService userLoginService;

	@Override
	public PushNotification addUpdatePushNotification(final PushNotification pushNotification) {
		return pushNotificationRepository.save(pushNotification);
	}

	@Override
	public PushNotification getPushNotificationDetail(final Long id) throws NotFoundException {
		return pushNotificationRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("notification.not.found", new Object[] { id })));
	}

	@Override
	public List<PushNotification> getPushNotificationListForUser(final Integer startIndex, final Integer pageSize)
			throws NotFoundException, ValidationException {
		UserLogin userLoginReceiver;
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.USER.name().equals(userLogin.getEntityType())) {
			userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
		} else {
			userLoginReceiver = userLogin;
		}
		return pushNotificationRepository.findAllByReceiverId(userLoginReceiver.getId(), startIndex, pageSize);
	}

	@Override
	public Long getCountOfPushNotificationForUser() throws NotFoundException, ValidationException {
		UserLogin userLoginReceiver;
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.USER.name().equals(userLogin.getEntityType())) {
			userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
		} else {
			userLoginReceiver = userLogin;
		}
		return pushNotificationRepository.countOfPushNotificationByReceiver(userLoginReceiver.getId());
	}

	@Override
	public void deletePushNotification(final Long pushNotificationId) throws NotFoundException, ValidationException {
		PushNotification pushNotification = getPushNotificationDetail(pushNotificationId);
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.USER.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage("can.not.delete.notification", null));
		}
		/**
		 * Get all push notification receiver list for this push notification and receiver
		 */
		Optional<List<PushNotificationReceiver>> pushNotificationReceiverList = pushNotificationReceiverRepository
				.findAllByReceiverIdAndPushNotificationId(userLogin.getId(), pushNotification.getId());
		/**
		 * Delete all push notification receiver list for this push notification and receiver
		 */
		if (pushNotificationReceiverList.isPresent()) {
			pushNotificationReceiverRepository.deleteAll(pushNotificationReceiverList.get());
		}
		/**
		 * Get all push notification receiver list for this push notification
		 */
		Optional<List<PushNotificationReceiver>> pushNotificationReceiverListForPushNotification = pushNotificationReceiverRepository
				.findAllByPushNotificationId(pushNotificationId);
		/**
		 * if push notification receiver list not present then delete this push notification
		 */
		if (!pushNotificationReceiverListForPushNotification.isPresent()) {
			pushNotificationRepository.delete(pushNotification);
		}
	}

	@Override
	public Long getTodaysPushNotificationCountForUser() throws NotFoundException, ValidationException {
		UserLogin userLoginReceiver;
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.USER.name().equals(userLogin.getEntityType())) {
			userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
		} else {
			userLoginReceiver = userLogin;
		}
		return pushNotificationRepository.countOfTodaysPushNotificationByReceiver(userLoginReceiver.getId());
	}
}
