package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.PushNotificationReceiver;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
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
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	PushNotificationReceiver getPushNotificationReceiverDetail(Long id) throws NotFoundException;

	/**
	 * Get push notification list for user
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  deviceId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Page<PushNotificationReceiver> getPushNotificationListForUser(Integer pageNumber, Integer pageSize, String deviceId)
			throws NotFoundException, ValidationException;

	/**
	 * delete push notification receiver by id
	 *
	 * @param  pushNotificationReceiverId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deletePushNotificationReceiver(Long pushNotificationReceiverId) throws NotFoundException, ValidationException;
}
