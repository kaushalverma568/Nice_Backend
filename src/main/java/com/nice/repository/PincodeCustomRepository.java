/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.model.Pincode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
public interface PincodeCustomRepository {

	/**
	 * Get List of pincode based on filter parameters with pagination
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  cityId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	List<Pincode> getPincodeListBasedOnParams(Integer startIndex, Integer pageSize, Boolean activeRecords, Long cityId, String searchKeyword);

	/**
	 * Get count of pincode based on filter parameters
	 *
	 * @param  activeRecords
	 * @param  cityId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	Long getPincodeCountBasedOnParams(Boolean activeRecords, Long cityId, String searchKeyword);

}
