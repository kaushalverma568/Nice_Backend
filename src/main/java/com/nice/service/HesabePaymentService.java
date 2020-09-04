/**
 *
 */
package com.nice.service;

import com.nice.dto.VendorPaymentDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Aug-2020
 */
public interface HesabePaymentService {
	/**
	 * for vendor subscription plan purchase and generate url
	 *
	 * @param vendorPaymentDTO
	 * @return
	 */
	String createPaymentGatewayVendor(VendorPaymentDTO vendorPaymentDTO);

	/**
	 * for decrypt hesabe encrypted string
	 *
	 * @param encrypted
	 * @return
	 */
	String decrypt(String encrypted);

}
