/**
 *
 */
package com.nice.service;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Aug-2020
 */
public interface HesabePaymentService {
	/**
	 * for Hesabe to generate payment url
	 *
	 * @param orderId
	 * @param amount
	 * @return
	 */
	String createPaymentGateway(String orderId, Double amount, String redirectUrl);

	/**
	 * for decrypt hesabe encrypted string
	 *
	 * @param encrypted
	 * @return
	 */
	String decrypt(String encrypted);

}
