/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.model.DeliveryBoy;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 2, 2020
 */
public interface DeliveryBoyCustomRepository {

	/**
	 * Get List of deliveryBoy based on filter parameters with pagination
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  deliveryBoyFilterDTO
	 * @return
	 */
	List<DeliveryBoy> getDeliveryBoyListBasedOnParams(Integer startIndex, Integer pageSize, DeliveryBoyFilterDTO deliveryBoyFilterDTO);

	/**
	 * Get count of deliveryBoy based on filter parameters
	 *
	 * @param  deliveryBoyFilterDTO
	 * @return
	 */
	Long getDeliveryBoyCountBasedOnParams(DeliveryBoyFilterDTO deliveryBoyFilterDTO);

}
