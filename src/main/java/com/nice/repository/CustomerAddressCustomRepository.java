/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.model.CustomerAddress;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public interface CustomerAddressCustomRepository {

	/**
	 * Get customer list based on parameters
	 *
	 * @param  customerId
	 * @param  countryId
	 * @param  stateId
	 * @param  cityId
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<CustomerAddress> getCustomerAddressListBasedOnParams(Boolean activeRecords, Long customerId, Long countryId, Long stateId, Long cityId, Long pincode,
			Integer startIndex, Integer pageSize);

}
