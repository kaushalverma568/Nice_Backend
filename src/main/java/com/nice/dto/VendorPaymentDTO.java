package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 04-Sep-2020
 */
@Data
public class VendorPaymentDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2697011396995137084L;

	private Long id;
	private String vendorOrderId;
	private Long vendorId;
	private Long subscriptionPlanId;
	private String paymentToken;
	private String paymentId;
	private Double administrativeCharge;
	private String status;
	private Boolean active;
	private Double amount;
}
