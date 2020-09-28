package com.nice.service.impl;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.PushNotification;
import com.nice.repository.PushNotificationRepository;
import com.nice.service.PushNotificationService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
@Service(value = "pushNotificationService")
@Transactional(rollbackOn = Throwable.class)
public class PushNotificationServiceImpl implements PushNotificationService {

	@Autowired
	private PushNotificationRepository pushNotificationRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public PushNotification addUpdatePushNotification(final PushNotification pushNotification) {
		return pushNotificationRepository.save(pushNotification);
	}

	@Override
	public PushNotification getPushNotificationDetail(final Long id) throws NotFoundException {
		return pushNotificationRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("notification.not.found", new Object[] { id })));
	}

}
