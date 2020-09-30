package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.PushNotificationReceiver;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
@Repository
public interface PushNotificationReceiverRepository extends JpaRepository<PushNotificationReceiver, Long> {
	/**
	 * get push notification receiver list by push notification id and receiver id
	 *
	 * @param  pushNotificationId
	 * @return
	 */
	Optional<List<PushNotificationReceiver>> findAllByReceiverIdAndPushNotificationId(Long receiverId, Long pushNotificationId);

	/**
	 * get push notification receiver list by push notification id
	 *
	 * @param  pushNotificationId
	 * @return
	 */
	Optional<List<PushNotificationReceiver>> findAllByPushNotificationId(Long pushNotificationId);
}