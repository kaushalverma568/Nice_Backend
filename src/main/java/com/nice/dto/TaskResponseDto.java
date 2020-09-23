/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
@Data
public class TaskResponseDto implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 1238867748740105555L;

	private Long orderId;
	private Long id;
	private Double orderAmount;
	private String paymentMode;
	private Double orderAmtWithoutDeliveryCharge;
	private Date orderDate;
	private Date deliveredDate;
	/**
	 * Details related to vendor
	 */
	private String vendorName;
	private String vendorPhoneNumber;
	private String vendorAddress;
	private BigDecimal pickupLatitude;
	private BigDecimal pickupLongitude;

	/**
	 * Details related to customer
	 */
	private String customerName;
	private String customerAddress;
	private String customerPhoneNumber;
	private Long deliveryLatitude;
	private Long deliveryLongitude;

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
	 * Details related to Task
	 */
	private String taskType;
	private Date createdAt;
	private String orderStatus;

}
