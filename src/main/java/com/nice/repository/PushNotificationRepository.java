package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.PushNotification;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Sep-2020
 */
@Repository
public interface PushNotificationRepository extends JpaRepository<PushNotification, Long> {

	/**
	 * Get push notification list by receiver id
	 *
	 * @param
	 * @return
	 */
	@Query(value = "Select pn.* from push_notification pn join push_notification_receiver pnr on pnr.push_notification_id=pn.id where pnr.receiver_id = :receiverId group by (pn.id) order by pn.id desc offset :offset limit :limit", nativeQuery = true)
	List<PushNotification> findAllByReceiverId(Long receiverId, Integer offset, Integer limit);

	/**
	 * Get count of push notification list by receiver id
	 *
	 * @param
	 * @return
	 */
	@Query(value = "select count (total) from (Select pn.* from push_notification pn join push_notification_receiver pnr on pnr.push_notification_id=pn.id where pnr.receiver_id = :receiverId group by (pn.id))as total", nativeQuery = true)
	Long countOfPushNotificationByReceiver(Long receiverId);

}
