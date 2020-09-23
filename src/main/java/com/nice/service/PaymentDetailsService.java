package com.nice.service;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.DeliveryBoyPayoutDTO;
import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.PaymentDetails;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
public interface PaymentDetailsService {

	/**
	 * persist paymentDetails object
	 *
	 * @param  paymentDetailsDTO
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	void addPaymentDetails(PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * get DTO object of paymentDetails
	 *
	 * @param  paymentDetailsId
	 * @return
	 * @throws NotFoundException
	 */
	PaymentDetailsResponseDTO getPaymentDetails(Long paymentDetailsId) throws NotFoundException;

	/**
	 * check paymentDetails duplication and returning Boolean value.
	 *
	 * @param  paymentDetailsDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isPaymentDetailsExists(PaymentDetailsDTO paymentDetailsDTO);

	/**
	 * get detail object of paymentDetails
	 *
	 * @param  paymentDetailsId
	 * @return
	 * @throws NotFoundException
	 */
	PaymentDetails getPaymentDetailsDetail(Long paymentDetailsId) throws NotFoundException;

	/**
	 * get payment history
	 *
	 * @param  vendorId
	 * @param  deliveryBoyId
	 * @param  fromDate
	 * @param  toDate
	 * @param  pageSize
	 * @param  pageNumber
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Page<PaymentDetails> getPaymentHistory(Long deliveryBoyId, Long vendorId, Date fromDate, Date toDate, Integer pageNumber, Integer pageSize)
			throws ValidationException, NotFoundException;

	/**
	 * Get all delivery boy's payout details
	 *
	 * @param  searchId
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @param  pageSize
	 * @param  startIndex
	 * @return
	 */
	List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(Long searchId, Long deliveryBoyId, Date registeredOn, Integer startIndex, Integer pageSize);

	/**
	 * Get delivery boy's payout count based on param
	 *
	 * @param  searchId
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @return
	 */
	Long getDeliveryBoyPayoutCountBasedOnParam(Long searchId, Long deliveryBoyId, Date registeredOn);
}
