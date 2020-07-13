package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class OrderToppingsDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1576006448308011967L;
	private Long id;
	private Long productToppingsId;
	private String toppingsName;
	private Long orderItemId;
	private Long quantity;
	private Boolean active;
	private Double amount;
	private Double discountedAmount;

	/**
	 * @param productToppingsId
	 * @param orderItemId
	 * @param quantity
	 * @param active
	 */
	public OrderToppingsDto(final Long productToppingsId, final Long orderItemId, final Long quantity, final Boolean active, final Double amount,
			final Double discountedAmount) {
		super();
		this.productToppingsId = productToppingsId;
		this.orderItemId = orderItemId;
		this.quantity = quantity;
		this.active = active;
		this.amount = amount;
		this.discountedAmount = discountedAmount;
	}

	/**
	 *
	 */
	public OrderToppingsDto() {
	}
}
