
package com.nice.repository;

import java.util.List;

import com.nice.dto.StockDetailFilterDTO;
import com.nice.model.StockDetails;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Feb-2020
 */
public interface StockDetailsCustomRepository {

	/**
	 * list by filter
	 * 
	 * @param startIndex
	 * @param pageSize
	 * @param stockDetailFilterDTO
	 * @return
	 */
	List<StockDetails> getStockDetailsListForSerachString(Integer startIndex, Integer pageSize, StockDetailFilterDTO stockDetailFilterDTO);

	/**
	 * count by filter
	 * 
	 * @param stockDetailFilterDTO
	 * @return
	 */
	Long getStockDetailsListForSerachStringCount(StockDetailFilterDTO stockDetailFilterDTO);

}
