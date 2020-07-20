/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Jul-2020
 */
@Data
@EqualsAndHashCode
public class OrdersResponseDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -8799119443604713278L;

	private Long id;

	private String customerName;
	private String phoneNumber;
	private Double totalOrderAmount;
	private String paymentMode;
	private String orderStatus;
	private Date createdAt;
	private Date deliveryDate;
	private Date replacementDate;
	private Date replacementReqDate;
	private String vendorName;
	private String address;
	private Double deliveryCharge;
	private String deliveryBoyName;
	private String cancelReason;
	private Date orderDate;
	private List<OrderItemResponseDTO> orderItemResponseDtoList;
	private Long count;
	private String description;
	private String returnReplaceReason;
	private String email;
	private Long vendorId;
	private List<OrderStatusDto> orderStatusDtoList;
	private String replacementDeliveryBoyName;
	private String vendorImageUrl;
	/**
	 * city field is added for set city in email templates
	 */
	private String city;
	/**
	 * pincode field is added for set pincode in email and push notification
	 * templates
	 */
	private String pincode;

}
