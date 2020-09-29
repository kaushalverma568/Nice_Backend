package com.nice.service;

import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.springframework.data.domain.Page;

import com.nice.dto.DeliveryBoyPayoutDTO;
import com.nice.dto.PayableAmountDTO;
import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.dto.VendorPayoutDTO;
import com.nice.exception.FileOperationException;
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

	Long addPaymentDetails(PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException, ValidationException;

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
	List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(Long deliveryBoyId, Date registeredOn, Integer startIndex, Integer pageSize);

	/**
	 * Get delivery boy's payout count based on param
	 *
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @return
	 */
	Long getDeliveryBoyPayoutCountBasedOnParam(Long deliveryBoyId, Date registeredOn);

	/**
	 * Get vendor's payout count based on param
	 *
	 * @param  vendorId
	 * @param  businessCategoryId
	 * @return
	 */
	Long getVendorPayoutCountBasedOnParam(Long vendorId, Long businessCategoryId);

	/**
	 * Get all vendor's payout details
	 *
	 * @param  vendorId
	 * @param  businessCategoryId
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<VendorPayoutDTO> getVendorPayout(Long vendorId, Long businessCategoryId, Integer startIndex, Integer pageSize);

	/**
	 * get payable amount for task list
	 *
	 * @param  payableAmountDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Double getPayableAmountForTaskList(@Valid PayableAmountDTO payableAmountDTO) throws NotFoundException, ValidationException;

	/**
	 * Export payment history
	 *
	 * @param  httpServletResponse
	 * @param  deliveryBoyId
	 * @param  vendorId
	 * @param  fromDate
	 * @param  toDate
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	void exportPaymentHistory(HttpServletResponse httpServletResponse, Long deliveryBoyId, Long vendorId, Date fromDate, Date toDate)
			throws NotFoundException, ValidationException, FileOperationException;

	/**
	 * Export vendor payout
	 *
	 * @param  vendorId
	 * @param  businessCategoryId
	 * @param  httpServletResponse
	 * @throws FileOperationException
	 */
	void exportVendorPayout(Long vendorId, Long businessCategoryId, HttpServletResponse httpServletResponse) throws FileOperationException;

	/**
	 * Export delivery boy payout
	 *
	 * @param  deliveryBoyId
	 * @param  registeredOn
	 * @param  httpServletResponse
	 * @throws FileOperationException
	 */
	void exportDeliveryBoyPayout(Long deliveryBoyId, Date registeredOn, HttpServletResponse httpServletResponse) throws FileOperationException;

	/**
	 * Send Email to delivery boy/vendor after payout
	 *
	 * @param  entityType
	 * @param  paymentDetailsId
	 * @throws NotFoundException
	 */
	void sendEmailAfterPayout(String entityType, Long paymentDetailsId) throws NotFoundException;

	/**
	 * Send push notification to delivery boy/vendor after payout
	 *
	 * @param  entityType
	 * @param  paymentDetailsId
	 * @throws NotFoundException
	 */
	void sendPushNotificationAfterPayout(String entityType, Long paymentDetailsId) throws NotFoundException;
}
