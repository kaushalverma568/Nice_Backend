package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Data
public class TempCartExtrasDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6352658540980485286L;
	private Long id;
	private Long productExtrasId;
	private Long tempCartItemId;
	private Long quantity;
	private Boolean active;

	/**
	 * @param productExtrasId
	 * @param tempCartItemId
	 * @param quantity
	 * @param active
	 */
	public TempCartExtrasDto(final Long productExtrasId, final Long tempCartItemId, final Long quantity, final Boolean active) {
		super();
		this.productExtrasId = productExtrasId;
		this.tempCartItemId = tempCartItemId;
		this.quantity = quantity;
		this.active = active;
	}

	/**
	 *
	 */
	public TempCartExtrasDto() {
	}
}
