package com.nice.service;

import java.util.List;

import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
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

	/**
	 * Get push notification list for user
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<PushNotification> getPushNotificationListForUser(Integer pageNumber, Integer pageSize) throws NotFoundException, ValidationException;

	/**
	 * delete push notification by id
	 *
	 * @param  pushNotificationId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deletePushNotification(Long pushNotificationId) throws NotFoundException, ValidationException;

	Long getCountOfPushNotificationForUser() throws NotFoundException, ValidationException;

}
