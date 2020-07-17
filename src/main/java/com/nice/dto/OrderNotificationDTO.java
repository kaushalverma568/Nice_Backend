package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 15-07-2020
 */
@Data
public class OrderNotificationDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -8400416291748265071L;
	/**
	 * order id
	 */
	private Long id;

	private String orderStatus;

	private String pickUpAddress;

	private String dropAddress;

	private String paymentMode;

	private Double totalOrderAmount;

	private Double distance;

}