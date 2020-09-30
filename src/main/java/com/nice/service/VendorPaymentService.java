package com.nice.service;

import java.util.List;

import com.nice.dto.VendorPaymentDTO;
import com.nice.dto.VendorPaymentResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.VendorPayment;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 04-Sep-2020
 */
public interface VendorPaymentService {
	/**
	 * add vendor payment
	 *
	 * @param vendorPaymentDTO
	 * @throws NotFoundException
	 */
	void addVendorPayment(VendorPaymentDTO vendorPaymentDTO) throws NotFoundException;

	/**
	 * update vendor payment
	 *
	 * @param vendorPayment
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateVendorPayment(VendorPayment vendorPayment) throws NotFoundException, ValidationException;

	/**
	 * get vendor payment by orderId
	 *
	 * @param vendorOrderId
	 * @return
	 * @throws NotFoundException
	 */
	VendorPayment getVendorPaymentByVendorOrderId(String vendorOrderId) throws NotFoundException;

	/**
	 * get vendor payment by id
	 *
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	VendorPayment getVendorPaymentById(Long id) throws NotFoundException;

	/**
	 * get vendor payment dto by orderid
	 *
	 * @param vendorOrderId
	 * @return
	 * @throws NotFoundException
	 */
	VendorPaymentDTO getVendorPaymentDTOByVendorOrderId(String vendorOrderId) throws NotFoundException;

	/**
	 * get vendor payment by vendor order id and status
	 *
	 * @param vendorOrderId
	 * @param status
	 * @return
	 * @throws NotFoundException
	 */
	VendorPayment getVendorPaymentByVendorOrderIdAndStatus(String vendorOrderId, String status) throws NotFoundException;

	/**
	 * get vendor payment history
	 *
	 * @param vendorId
	 * @return
	 */
	List<VendorPaymentResponseDTO> getVendorPaymentListByVendorId(Long vendorId);

	/**
	 * get latest payment of vendor
	 *
	 * @param vendorId
	 * @param subscriptionPlanId
	 * @return
	 */
	VendorPayment getLatestVendorPaymentByVendorIdAndBusinessCategoryId(Long vendorId, Long subscriptionPlanId);
}
