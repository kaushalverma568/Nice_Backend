/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 17-07-2020
 */
@Data
public class DashBoardDetailDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3136066704912453173L;

	private Long deliveryBoyId;

	private Integer assignedOrdersCount;

	private Integer deliveredOrdersCount;

	private Double todaysCashCollection;

	/**
	 * is delivery boy available (is active in UI)
	 */
	private Boolean isAvailable;

	private Long onGoingOrderId;

	private Long onGoingTaskId;

	private String orderType;
}
