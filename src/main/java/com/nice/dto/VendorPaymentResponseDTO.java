package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
@Data
public class VendorPaymentResponseDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = -6606950877418591620L;

	private Long id;

	private String vendorOrderId;

	private String vendorName;

	private Long vendorId;

	private String vendorStoreName;

	private String subscriptionPlanName;

	private Long subscriptionPlanId;

	private String paymentToken;

	private String paymentId;

	private Double administrativeCharge;

	private String status;

	private Double amountPaid;

	private Double subscriptionPlanAmount;

	private Date paymentDate;
}
