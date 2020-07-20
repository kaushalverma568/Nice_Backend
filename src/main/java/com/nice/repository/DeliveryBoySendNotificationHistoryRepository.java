package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoySendNotificationHistory;

@Repository
public interface DeliveryBoySendNotificationHistoryRepository extends JpaRepository<DeliveryBoySendNotificationHistory, Long> {

	/**
	 * get delivery boy send notification history for delivery boy
	 *
	 * @param deliveryBoy
	 * @return
	 */
	Optional<DeliveryBoySendNotificationHistory> findByDeliveryBoy(DeliveryBoy deliveryBoy);

	/**
	 * get notification history list by order id
	 *
	 * @param orderId
	 */
	List<DeliveryBoySendNotificationHistory> findAllByOrderId(Long orderId);

	/**
	 * get delivery boy send notification history by delivery boy and order id
	 *
	 * @param deliveryBoy
	 * @return
	 */
	Optional<DeliveryBoySendNotificationHistory> findByDeliveryBoyAndOrderId(DeliveryBoy deliveryBoy, Long orderId);

}
