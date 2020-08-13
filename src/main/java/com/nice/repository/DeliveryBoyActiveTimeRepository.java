/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.Optional;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoyActiveTime;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Aug-2020
 */
@Repository(value = "deliveryBoyActiveTimeRepository")
public interface DeliveryBoyActiveTimeRepository extends CrudRepository<DeliveryBoyActiveTime, Long> {

	/**
	 * @param deliveryPersonId
	 */
	Optional<DeliveryBoyActiveTime> findAllByDeliveryBoyId(Long deliveryPersonId);

	/**
	 *
	 * @param deliveryPersonId
	 * @param recordDate
	 * @return
	 */
	Optional<DeliveryBoyActiveTime> findAllByDeliveryBoyIdAndRecordDate(Long deliveryPersonId, Date recordDate);
}
