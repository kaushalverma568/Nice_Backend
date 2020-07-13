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
public class OrderProductAttributeValueDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2885797757126099887L;
	private Long id;
	private Long productAttributeValueId;
	private Long orderItemId;
	private Long quantity;
	private Boolean active;
	private Double amount;
	private Double discountedAmount;
	private String attributeValue;
	private String attributeName;

	/**
	 * @param productAttributeValueId
	 * @param tempCartItemId
	 * @param quantity
	 * @param active
	 */
	public OrderProductAttributeValueDTO(final Long productAttributeValueId, final Long orderItemId, final Long quantity, final Boolean active,
			final Double amount, final Double discountedAmount) {
		super();
		this.productAttributeValueId = productAttributeValueId;
		this.orderItemId = orderItemId;
		this.quantity = quantity;
		this.active = active;
		this.amount = amount;
		this.discountedAmount = discountedAmount;
	}

	/**
	 *
	 */
	public OrderProductAttributeValueDTO() {
	}
}
