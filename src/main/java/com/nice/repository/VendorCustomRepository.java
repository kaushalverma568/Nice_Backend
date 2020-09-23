/**
 *
 */
package com.nice.repository;

import java.math.BigDecimal;
import java.util.List;

import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.model.Vendor;

/**
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

	/**
	 * get vendor list for customer based on parameters with pagination
	 *
	 * @param startIndex
	 * @param pageSize
	 * @param vendorListFilterDTO
	 * @return
	 */
	List<Vendor> getVendorListForCustomerBasedOnParams(Integer startIndex, Integer pageSize, VendorListFilterDTO vendorListFilterDTO);

	/**
	 * get count of vendor for customer based on filter parameters
	 *
	 * @param vendorListFilterDTO
	 * @return
	 */
	Long getVendorCountForCustomerBasedOnParams(VendorListFilterDTO vendorListFilterDTO);

	/**
	 * get vendor detail based on filters
	 *
	 * @param vendorListFilterDTO
	 * @return
	 */
	Double getVendorDistanceForCustomerBasedOnParams(final Long vendorId, final BigDecimal latitude, BigDecimal longitude);

}
