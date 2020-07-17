package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyCurrentStatus;

@Repository
public interface DeliveryBoyCurrentStatusRepository extends JpaRepository<DeliveryBoyCurrentStatus, Long> {

	/**
	 * Get delivery boy current status by delivery boy
	 * 
	 * @param deliveryBoy
	 * @return
	 */
	Optional<DeliveryBoyCurrentStatus> findByDeliveryBoy(DeliveryBoy deliveryBoy);
}
