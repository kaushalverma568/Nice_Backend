package com.nice.service;

import com.nice.exception.NotFoundException;
import com.nice.model.PushNotification;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
public interface PushNotificationService {
	/**
	 * add push notification
	 *
	 * @param pushNotification
	 */
	PushNotification addUpdatePushNotification(PushNotification pushNotification);

	/**
	 * get Push notification by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	PushNotification getPushNotificationDetail(Long id) throws NotFoundException;
}
