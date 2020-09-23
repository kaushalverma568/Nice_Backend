/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.List;

import com.nice.dto.DeliveryBoyPayoutDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
public interface PaymentDetailsCustomRepository {

	/**
	 * Get delivery boy pay out based on params
	 *
	 * @param  searchId
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(Long searchId, Long deliveryBoyId, Date registeredOn, Integer startIndex, Integer pageSize);

	/**
	 * Get delivery boy pay out count based on params
	 *
	 * @param  searchId
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @return
	 */
	Long getDeliveryBoyPayoutCountBasedOnParam(Long searchId, Long deliveryBoyId, Date registeredOn);

}
