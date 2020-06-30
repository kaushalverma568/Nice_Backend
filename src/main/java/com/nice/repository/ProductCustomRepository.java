/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.dto.ProductParamRequestDTO;
import com.nice.model.Product;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 21-Jan-2020
 */
public interface ProductCustomRepository {

	/**
	 * @param productParamRequestDTO
	 * @return
	 */
	Long getProductCountBasedOnParams(ProductParamRequestDTO productParamRequestDTO);

	/**
	 * @param productParamRequestDTO
	 * @param startIndex
	 * @return
	 */
	List<Product> getProductListBasedOnParams(ProductParamRequestDTO productParamRequestDTO, Integer startIndex, Integer pageSize);

	/**
	 * get category wise product count list
	 *
	 * @return
	 */
	// List<CategoryWiseProductCountDTO> getCategoryWiseProductCountList();

}
