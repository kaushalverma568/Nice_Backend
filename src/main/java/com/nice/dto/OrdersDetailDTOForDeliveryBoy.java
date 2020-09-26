/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 4, 2020
 */
@Data
public class OrdersDetailDTOForDeliveryBoy implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5327575779094211103L;

	private Long id;

	private Long taskId;

	private String customerName;

	private String customerEmail;

	private String phoneNumber;

	private String paymentMode;

	private String taskStatus;

	private String address;

	private Double deliveryCharge;

	private Double totalOrderAmount;

	private Double grossOrderAmount;

	private String deliveryBoyName;

	private String deliveryBoyPhoneNumber;

	private String deliveryBoyEmail;

	private String storeName;

	private String storeImageUrl;

	private Date createdAt;

	private Date deliveryDate;

	private List<OrderItemResponseDTO> orderItemResponseDTOList;

	private String orderRequest;

	private String pickUpAddress;

	private String dropAddress;

	private BigDecimal pickupLatitude;

	private BigDecimal pickupLongitude;

	private BigDecimal dropLatitude;

	private BigDecimal dropLongitude;

	private String pickupContactName;

	private String pickupContactNo;

	private String dropContactName;

	private String dropContactNo;

	private Double distance;

	private Boolean cashCollected;

	private String nextStatus;

}
