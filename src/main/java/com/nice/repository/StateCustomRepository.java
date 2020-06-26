/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
public interface StateCustomRepository {

	/**
	 * @param  startIndex
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  countryId
	 * @param  searchKeyword
	 * @return
	 */
	List<State> getStateList(Integer startIndex, Integer pageSize, Boolean activeRecords, Long countryId, String searchKeyword);

	/**
	 * @param  activeRecords
	 * @param  countryId
	 * @param  searchKeyword
	 * @return
	 */
	Long getStateCountBasedonParams(Boolean activeRecords, Long countryId, String searchKeyword);

}
