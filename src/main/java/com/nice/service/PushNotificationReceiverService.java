package com.nice.service;

import com.nice.exception.NotFoundException;
import com.nice.model.PushNotificationReceiver;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Sep-2020
 */
public interface PushNotificationReceiverService {
	/**
	 * add push notification Receiver
	 *
	 * @param pushNotification
	 */
	PushNotificationReceiver addUpdatePushNotificationReceiver(PushNotificationReceiver pushNotificationReceiver);

	/**
	 * get Push notification receiver by id
	 *
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	PushNotificationReceiver getPushNotificationReceiverById(Long id) throws NotFoundException;
}
