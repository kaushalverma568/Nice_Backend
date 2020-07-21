/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 04-Apr-2020
 */
@Data
public class OrdersListDTOForDeliveryBoy implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 9586571908058744L;

	private Long id;

	private String customerName;

	private String paymentMode;

	private String taskStatus;

	private Long taskId;

	private String taskType;

	private String deliveryAddress;

	private Integer totalQty;
	/**
	 * this is for collection list in delivery boy app
	 */
	private Double orderAmount;
}
