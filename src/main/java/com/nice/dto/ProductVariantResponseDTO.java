/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Mar-2020
 * @description :
 */
@Data
public class ProductVariantResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1005574925308104048L;
	private Long id;
	private Long productId;
	private String productName;
	private Long uomId;
	private String uomMeasurement;
	private Double uomQuantity;
	private String uomLabel;
	private Double discountedRate;
	private Double rate;
	private String sku;
	/**
	 * This field is populated from inventory.
	 */
	private Integer availableQty;
	private Boolean active;

	private Boolean productAvailable;

	public ProductVariantResponseDTO() {

	}

	/**
	 * @param id
	 * @param productId
	 * @param productName
	 * @param uomLabel
	 * @param availableQty
	 */
	public ProductVariantResponseDTO(final Long id, final Long productId, final String productName, final String uomLabel, final Integer availableQty) {
		super();
		this.id = id;
		this.productId = productId;
		this.productName = productName;
		this.uomLabel = uomLabel;
		this.availableQty = availableQty;
	}

}
