/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 04-Apr-2020
 */
@Data
public class OrdersDetailDTOForDeliveryBoy implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5327575779094211103L;

	private Long id;

	private String customerName;

	private String customerContact;

	private String paymentMode;

	private String deliveryAddress;

	private Double totalOrderAmount;

	private String taskStatus;

	private List<Date> attemptedAt;

	private List<OrderItemDTOForDeliveryBoy> orderItemDTOForDeliveryBoyList;

	/**
	 * added for delivery log
	 */
	private String products;
}
