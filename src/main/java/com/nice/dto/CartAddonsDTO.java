package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class CartAddonsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7406022628390929149L;
	private Long id;
	private Long productAddonsId;
	private String name;
	private Long quantity;
	private Long tempCartId;
	private Boolean active;

	/**
	 * @param productAddonsId
	 * @param quantity
	 * @param tempCartId
	 * @param active
	 */
	public CartAddonsDTO(final Long productAddonsId, final Long tempCartId, final Long quantity, final Boolean active) {
		super();
		this.productAddonsId = productAddonsId;
		this.quantity = quantity;
		this.tempCartId = tempCartId;
		this.active = active;
	}

	public CartAddonsDTO() {
	}
}
