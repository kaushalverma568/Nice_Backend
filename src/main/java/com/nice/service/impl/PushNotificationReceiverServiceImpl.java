package com.nice.service.impl;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.UserType;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.PushNotificationReceiver;
import com.nice.model.UserLogin;
import com.nice.repository.PushNotificationReceiverRepository;
import com.nice.service.PushNotificationReceiverService;
import com.nice.service.UserLoginService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
@Service(value = "pushNotificationReceiverService")
@Transactional(rollbackOn = Throwable.class)
public class PushNotificationReceiverServiceImpl implements PushNotificationReceiverService {

	@Autowired
	private PushNotificationReceiverRepository pushNotificationReceiverRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UserLoginService userLoginService;

	@Override
	public PushNotificationReceiver addUpdatePushNotificationReceiver(final PushNotificationReceiver pushNotificationReceiver) {
		return pushNotificationReceiverRepository.save(pushNotificationReceiver);
	}

	@Override
	public PushNotificationReceiver getPushNotificationReceiverDetail(final Long id) throws NotFoundException {
		return pushNotificationReceiverRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("notification.receiver.not.found", new Object[] { id })));
	}

	@Override
	public Page<PushNotificationReceiver> getPushNotificationListForUser(final Integer pageNumber, final Integer pageSize, final String deviceId)
			throws NotFoundException, ValidationException {
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deviceId)) {
			throw new ValidationException(messageByLocaleService.getMessage("device.id.not.null", null));
		}
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC, "id"));
		UserLogin userLoginReceiver;
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.USER.name().equals(userLogin.getEntityType())) {
			userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
		} else {
			userLoginReceiver = userLogin;
		}
		return pushNotificationReceiverRepository.findAllByReceiverIdAndDeviceId(userLoginReceiver.getId(), deviceId, pageable);
	}

	@Override
	public void deletePushNotificationReceiver(final Long pushNotificationReceiverId) throws NotFoundException, ValidationException {
		PushNotificationReceiver pushNotificationReceiver = getPushNotificationReceiverDetail(pushNotificationReceiverId);
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (!userLogin.getId().equals(pushNotificationReceiver.getReceiverId())) {
			throw new ValidationException(messageByLocaleService.getMessage("can.not.delete.notification", null));
		}
		pushNotificationReceiverRepository.delete(pushNotificationReceiver);
	}
}
