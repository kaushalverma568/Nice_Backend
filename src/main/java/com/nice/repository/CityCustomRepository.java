/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Set;

import com.nice.model.City;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
public interface CityCustomRepository {

	/**
	 * Get List of city based on filter parameters with pagination
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  stateId
	 * @param  searchKeyword
	 * @return
	 */
	List<City> getCityListBasedOnParams(Integer startIndex, Integer pageSize, Boolean activeRecords, Long stateId, String searchKeyword, Set<Long> idsIn);

	/**
	 * Get count of city based on filter parameters
	 *
	 * @param  activeRecords
	 * @param  stateId
	 * @param  searchKeyword
	 * @param  idsIn
	 * @return
	 */
	Long getCityCountBasedOnParams(Boolean activeRecords, Long stateId, String searchKeyword, Set<Long> idsIn);

}
