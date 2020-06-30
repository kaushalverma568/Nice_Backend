/**
 *
 */
package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.DeliveryBoyLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeliveryBoyLocation;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 19, 2020
 */
public interface DeliveryBoyLocationService {

	/**
	 * add delivery boy location
	 *
	 * @param deliveryBoyLocationDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addDeliveryBoyLocation(DeliveryBoyLocationDTO deliveryBoyLocationDTO) throws ValidationException, NotFoundException;

	/**
	 * update delivery boy location
	 *
	 * @param deliveryBoyLocationDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateDeliveryBoyLocation(DeliveryBoyLocationDTO deliveryBoyLocationDTO) throws ValidationException, NotFoundException;

	/**
	 * get delivery boy location by delivery boy location id
	 *
	 * @param deliveryBoyLocationId
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyLocationDTO getDeliveryBoyLocation(Long deliveryBoyLocationId) throws NotFoundException;

	/**
	 * get delivery boy location detail by delivery boy location id
	 *
	 * @param deliveryBoyLocationId
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyLocation getDeliveryBoyLocationDetails(Long deliveryBoyLocationId) throws NotFoundException;

	/**
	 * get delivery boy's latest location detail by delivery boy
	 *
	 * @param deliveryBoy
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyLocation getDeliveryBoyLatestLocation(Long deliveryBoyId) throws NotFoundException;

	/**
	 * get delivery boy location list
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	Page<DeliveryBoyLocation> getDeliveryBoyLocationList(Integer pageNumber, Integer pageSize, Long deliveryBoyId) throws NotFoundException;

}
