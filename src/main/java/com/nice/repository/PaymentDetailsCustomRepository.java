/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.List;

import com.nice.dto.DeliveryBoyPayoutDTO;
import com.nice.dto.VendorPayoutDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
public interface PaymentDetailsCustomRepository {

	/**
	 * Get delivery boy pay out based on params
	 *
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(Long deliveryBoyId, Date registeredOn, Integer startIndex, Integer pageSize);

	/**
	 * Get delivery boy pay out count based on params
	 *
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @return
	 */
	Long getDeliveryBoyPayoutCountBasedOnParam(Long deliveryBoyId, Date registeredOn);

	/**
	 * Get vendor pay out count based on params
	 *
	 * @param  vendorId
	 * @param  businessCategoryId
	 * @return
	 */
	Long getVendorPayoutCountBasedOnParam(Long vendorId, Long businessCategoryId);

	/**
	 * Get vendor pay out based on params
	 *
	 * @param  vendorId
	 * @param  businessCategoryId
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<VendorPayoutDTO> getVendorPayout(Long vendorId, Long businessCategoryId, Integer startIndex, Integer pageSize);

}
