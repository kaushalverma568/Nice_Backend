package com.nice.service;

import java.util.Date;
import java.util.List;

import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.PaymentDetails;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
public interface PaymentDetailsService {

	/**
	 * persist paymentDetails object
	 *
	 * @param paymentDetailsDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	void addPaymentDetails(PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * get DTO object of paymentDetails
	 *
	 * @param paymentDetailsId
	 * @return
	 * @throws NotFoundException
	 */
	PaymentDetailsResponseDTO getPaymentDetails(Long paymentDetailsId) throws NotFoundException;

	/**
	 * check paymentDetails duplication and returning Boolean value.
	 *
	 * @param paymentDetailsDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isPaymentDetailsExists(PaymentDetailsDTO paymentDetailsDTO);

	/**
	 * get detail object of paymentDetails
	 *
	 * @param paymentDetailsId
	 * @return
	 * @throws NotFoundException
	 */
	PaymentDetails getPaymentDetailsDetail(Long paymentDetailsId) throws NotFoundException;

	/**
	 * get payment history
	 *
	 * @param fromDate
	 * @param toDate
	 * @return
	 * @throws NotFoundException
	 */
	List<PaymentDetails> getPaymentHistory(Date fromDate, Date toDate);
}
