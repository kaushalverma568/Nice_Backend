package com.nice.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.PushNotificationReceiver;
import com.nice.repository.PushNotificationReceiverRepository;
import com.nice.service.PushNotificationReceiverService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
@Service(value = "pushNotificationReceiverService")
@Transactional(rollbackFor = Throwable.class)
public class PushNotificationReceiverServiceImpl implements PushNotificationReceiverService {

	@Autowired
	private PushNotificationReceiverRepository pushNotificationReceiverRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public PushNotificationReceiver addUpdatePushNotificationReceiver(final PushNotificationReceiver pushNotificationReceiver) {
		return pushNotificationReceiverRepository.save(pushNotificationReceiver);
	}

	@Override
	public PushNotificationReceiver getPushNotificationReceiverDetail(final Long id) throws NotFoundException {
		return pushNotificationReceiverRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("notification.receiver.not.found", new Object[] { id })));
	}

}
