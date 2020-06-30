/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.dto.VendorFilterDTO;
import com.nice.model.Vendor;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
 */
public interface VendorCustomRepository {

	/**
	 * Get List of vendor based on filter parameters with pagination
	 *
	 * @param startIndex
	 * @param pageSize
	 * @param vendorFilterDTO
	 * @return
	 */
	List<Vendor> getVendorListBasedOnParams(Integer startIndex, Integer pageSize, VendorFilterDTO vendorFilterDTO);

	/**
	 * Get count of vendor based on filter parameters
	 *
	 * @param vendorFilterDTO
	 * @return
	 */
	Long getVendorCountBasedOnParams(VendorFilterDTO vendorFilterDTO);

}
