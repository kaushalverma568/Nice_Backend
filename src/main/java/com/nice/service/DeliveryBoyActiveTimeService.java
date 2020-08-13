/**
 *
 */
package com.nice.service;

import com.nice.exception.NotFoundException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Aug-2020
 */
public interface DeliveryBoyActiveTimeService {

	/**
	 * @param deliveryPersonId
	 * @throws NotFoundException
	 */
	void updateDeliveryActiveTime(Long deliveryPersonId) throws NotFoundException;

	/**
	 *
	 * Returns the active time of delivery boy in minutes
	 *
	 * @param deliveryPersonId
	 * @return
	 * @throws NotFoundException
	 */
	Long getDeliveryBoyActiveTimeDetailsForToday(Long deliveryPersonId) throws NotFoundException;

}
