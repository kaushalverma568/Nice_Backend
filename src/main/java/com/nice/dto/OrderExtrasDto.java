package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class OrderExtrasDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6352658540980485286L;
	private Long id;
	private Long productExtrasId;
	private Long orderItemId;
	private Long quantity;
	private Boolean active;
	private Double amount;
	private Double discountedAmount;
	private String extrasName;

	/**
	 * @param productExtrasId
	 * @param orderItemId
	 * @param quantity
	 * @param active
	 */
	public OrderExtrasDto(final Long productExtrasId, final Long orderItemId, final Long quantity, final Boolean active, final Double amount,
			final Double discountedAmount) {
		super();
		this.productExtrasId = productExtrasId;
		this.orderItemId = orderItemId;
		this.quantity = quantity;
		this.active = active;
		this.amount = amount;
		this.discountedAmount = discountedAmount;
	}

	/**
	 *
	 */
	public OrderExtrasDto() {
	}
}
