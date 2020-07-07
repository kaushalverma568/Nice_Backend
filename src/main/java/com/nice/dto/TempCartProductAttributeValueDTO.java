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
public class TempCartProductAttributeValueDTO implements Serializable {

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
	 * @param tempCartItemId
	 * @param quantity
	 * @param active
	 */
	public TempCartProductAttributeValueDTO(final Long productAttributeValueId, final Long tempCartItemId, final Long quantity, final Boolean active) {
		super();
		this.productAttributeValueId = productAttributeValueId;
		this.tempCartItemId = tempCartItemId;
		this.quantity = quantity;
		this.active = active;
	}

	/**
	 *
	 */
	public TempCartProductAttributeValueDTO() {
	}
}
