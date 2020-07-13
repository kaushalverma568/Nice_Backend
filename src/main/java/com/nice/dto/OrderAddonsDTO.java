package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class OrderAddonsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7406022628390929149L;
	private Long id;
	private Long productAddonsId;
	private String addonsName;
	private Long quantity;
	private Long orderItemtId;
	private Boolean active;
	private Double amount;
	private Double discountedAmount;

	/**
	 * @param productAddonsId
	 * @param quantity
	 * @param orderItemId
	 * @param active
	 */
	public OrderAddonsDTO(final Long productAddonsId, final Long quantity, final Long orderItemId, final Boolean active, final Double amount,
			final Double discountedAmount) {
		super();
		this.productAddonsId = productAddonsId;
		this.quantity = quantity;
		this.orderItemtId = orderItemId;
		this.active = active;
		this.amount = amount;
		this.discountedAmount = discountedAmount;
	}

	public OrderAddonsDTO() {
	}
}
