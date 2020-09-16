package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
@Data
public class DeliveryLogDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 6185040669819902479L;

	private Long orderId;
	private Date orderDate;
	private String customerName;
	private Long customerId;
	private String customerEmail;
	private String deliveryBoyName;
	private Long deliveryBoyId;
	private String deliveryBoyEmail;
	private Date assignedDate;
	private String vendorName;
	private String vendorStoreName;
	private Long vendorId;
	private String taskStatus;
	private String taskType;
}
