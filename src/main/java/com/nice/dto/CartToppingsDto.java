package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class CartToppingsDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1576006448308011967L;
	private Long id;
	private Long productToppingsId;
	private String name;
	private Long tempCartItemId;
	private Long quantity;
	private Boolean active;

	/**
	 * @param productToppingsId
	 * @param tempCartItemId
	 * @param quantity
	 * @param active
	 */
	public CartToppingsDto(final Long productToppingsId, final Long tempCartItemId, final Long quantity, final Boolean active) {
		super();
		this.productToppingsId = productToppingsId;
		this.tempCartItemId = tempCartItemId;
		this.quantity = quantity;
		this.active = active;
	}

	/**
	 *
	 */
	public CartToppingsDto() {
	}
}
