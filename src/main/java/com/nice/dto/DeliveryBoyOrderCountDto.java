/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Data
public class DeliveryBoyOrderCountDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 843990639130302367L;
	private Long cartOrders;
	private Long returnOrders;
	private Long replaceOrders;
	private Double totalAmountPaid;
}
