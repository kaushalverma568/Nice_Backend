/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
@Data
public class TaskPayoutDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3636094706641382101L;

	private Long orderId;
	private Long id;
	private Double totalOrderAmount;
	private Double deliveryCharge;
	private Double adminCommission;
	private Double adminCommissionPercentage;
	private Double vendorPayableAmt;
	private String paymentMode;
	private Date orderDate;
	private Date deliveredDate;
	private String taskType;
	private Date createdAt;
	private String orderStatus;
	/**
	 * Details related to delivery boy payment
	 */
	private Long deliveryBoyPaymentDetailsId;
	private String deliveryBoyTransactionId;
	private Date deliveryBoyPaidOn;

	/**
	 * Details related to vendor payment
	 */
	private Long vendorPaymentDetailsId;
	private String vendorTransactionId;
	private Date vendorPaidOn;

	/**
	 * for export
	 */
	private String paymentStatus;
	private String orderType;
	private Date paidOn;
	private String transactionId;

}
