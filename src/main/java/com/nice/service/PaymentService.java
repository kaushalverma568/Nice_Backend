/**
 *
 */
package com.nice.service;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 18-Feb-2020
 */
public interface PaymentService {

	/**
	 * @param razorPayOrderId
	 * @param razorPayPaymentId
	 * @param razorPaySignature
	 * @return
	 */
	Boolean checkPaymentTransaction(String razorPayOrderId, String razorPayPaymentId, String razorPaySignature);

	/**
	 * @param razorpayOrderId
	 */
	void failedTransaction(String razorPayOrderId);

}
