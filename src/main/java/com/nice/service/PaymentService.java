/**
 *
 */
package com.nice.service;

import com.nice.dto.HesabePaymentDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 18-Feb-2020
 */
public interface PaymentService {

	/**
	 *
	 * @param hesabePaymentDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Boolean checkPaymentTransaction(HesabePaymentDTO hesabePaymentDTO) throws NotFoundException, ValidationException;

	/**
	 * @param razorpayOrderId
	 */
	void failedTransaction(String razorPayOrderId);

}
