/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class CartProductAttributeValueDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2885797757126099887L;
	private Long id;
	private Long productAttributeValueId;
	private Long tempCartItemId;
	private Long quantity;
	private Boolean active;

	/**
	 * @param productAttributeValueId
	 * @param cartItemId
	 * @param quantity
	 * @param active
	 */
	public CartProductAttributeValueDTO(final Long productAttributeValueId, final Long cartItemId, final Long quantity, final Boolean active) {
		super();
		this.productAttributeValueId = productAttributeValueId;
		this.tempCartItemId = cartItemId;
		this.quantity = quantity;
		this.active = active;
	}

	/**
	 *
	 */
	public CartProductAttributeValueDTO() {
	}
}
