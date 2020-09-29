package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyLocation;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Repository
public interface DeliveryBoyLocationRepository extends JpaRepository<DeliveryBoyLocation, Long> {

	/**
	 * @param  deliveryBoy
	 * @return
	 */
	Optional<DeliveryBoyLocation> findByDeliveryBoy(DeliveryBoy deliveryBoy);
}
