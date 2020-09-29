
package com.nice.repository;

import java.math.BigInteger;
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

	/**
	 * low stock count
	 * 
	 * @param vendorId
	 * @return
	 */
	Long getLowStockProductDetailsCount(Long vendorId);

	/**
	 * low stock list
	 * 
	 * @param vendorId
	 * @param startIndex
	 * @param pageSize
	 * @return
	 */
	List<BigInteger> getLowStockProductDetails(Long vendorId, Integer startIndex, Integer pageSize);

	/**
	 * 
	 * @param vendorId
	 * @param offset
	 * @param pageSize
	 * @return
	 */
	List<StockDetails> getExpiredStockDetails(Long vendorId, Integer startIndex, Integer pageSize);

	/**
	 * 
	 * @param vendorId
	 * @return
	 */
	Long getExpiredStockDetailsCount(Long vendorId);

}
