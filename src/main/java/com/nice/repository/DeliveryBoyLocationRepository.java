package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyLocation;

@Repository
public interface DeliveryBoyLocationRepository extends JpaRepository<DeliveryBoyLocation, Long> {

	/**
	 * get last updated delivery boy's location
	 *
	 * @param deliveryBoy
	 * @return
	 */
	Optional<DeliveryBoyLocation> findFirstByDeliveryBoyOrderByUpdatedAtDesc(DeliveryBoy deliveryBoy);

	/**
	 * get delivery boy's location list for delivery boy
	 *
	 * @param deliveryBoy
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoyLocation> findAllByDeliveryBoy(DeliveryBoy deliveryBoy, Pageable pageable);

}
