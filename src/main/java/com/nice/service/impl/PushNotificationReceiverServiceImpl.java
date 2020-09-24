package com.nice.service.impl;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.PushNotificationReceiver;
import com.nice.repository.PushNotificationReceiverRepository;
import com.nice.service.PushNotificationReceiverService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Sep-2020
 */
@Service(value = "pushNotificationReceiverService")
@Transactional(rollbackOn = Throwable.class)
public class PushNotificationReceiverServiceImpl implements PushNotificationReceiverService {

	@Autowired
	private PushNotificationReceiverRepository pushNotificationReceiverRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addUpdatePushNotificationReceiver(final PushNotificationReceiver pushNotificationReceiver) {
		pushNotificationReceiverRepository.save(pushNotificationReceiver);
	}

	@Override
	public PushNotificationReceiver getPushNotificationReceiverById(final Long id) throws NotFoundException {
		return pushNotificationReceiverRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("notification.receiver.not.found", new Object[] { id })));
	}

}
