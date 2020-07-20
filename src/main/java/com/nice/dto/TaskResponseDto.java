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
 * @date : 17-Jul-2020
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
	/**
	 * Details related to vendor
	 */
	private String vendorName;
	private String vendorContactNumber;
	private String vendorAddress;
	private BigDecimal pickupLatitude;
	private BigDecimal pickupLongitude;

	/**
	 * Details related to customer
	 */
	private String customerName;
	private String customerAddress;
	private String customerContactNumber;
	private Long deliveryLatitude;
	private Long deliveryLongitude;

	/**
	 * Details related to payment
	 */
	private Long paymentDetailsId;
	private String transactionId;
	private Date paidOn;

	/**
	 * Details related to Task
	 */
	private String taskType;
	private Date createdAt;
}
