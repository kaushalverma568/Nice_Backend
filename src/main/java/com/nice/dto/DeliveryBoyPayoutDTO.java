/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */

@Data
public class DeliveryBoyPayoutDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1772972357367784536L;

	private Long id;

	private String name;

	private String phoneNumber;

	private Date registeredOn;

	private Integer cartOrders;

	private Integer replacementOrders;

	private Integer returnOrders;

	private Integer totalAttended;

	private Date lastPaymentOn;

	private Double totalPaid;

	public DeliveryBoyPayoutDTO(final Long id, final String name, final String phoneNumber, final Date registeredOn, final Integer cartOrders,
			final Integer replacementOrders, final Integer returnOrders, final Integer totalAttended, final Date lastPaymentOn, final Double totalPaid) {
		super();
		this.id = id;
		this.name = name;
		this.phoneNumber = phoneNumber;
		this.registeredOn = registeredOn;
		this.cartOrders = cartOrders;
		this.replacementOrders = replacementOrders;
		this.returnOrders = returnOrders;
		this.totalAttended = totalAttended;
		this.lastPaymentOn = lastPaymentOn;
		this.totalPaid = totalPaid;
	}
}