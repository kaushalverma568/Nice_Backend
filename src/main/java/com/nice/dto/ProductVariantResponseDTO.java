/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
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
	private String measurement;
	private Double discountedRate;
	private Double rate;
	private String sku;
	/**
	 * This field is populated from inventory.
	 */
	private Integer availableQty;
	private Boolean active;

	/**
	 * Response only fields
	 */
	private String productNameEnglish;
	private String productNameArabic;
	private String uomMeasurementEnglish;
	private String uomMeasurementArabic;
	private Boolean productAvailable;

	private List<ProductToppingResponseDTO> productToppingsDtoList;
	private List<ProductAddonsDTO> productAddonsDtoList;

	private List<ProductAttributeResponseDTO> productAttributeValuesDtoList;

	/**
	 * Cart Impact of Product
	 */
	private List<Long> cartQtyList = new ArrayList<>();
	private List<Long> cartIdList = new ArrayList<>();

	/**
	 * Image details
	 */
	private String image;
	private String detailImage;

	public ProductVariantResponseDTO() {

	}

	/**
	 * @param id
	 * @param productId
	 * @param productName
	 * @param uomLabel
	 * @param availableQty
	 */
	public ProductVariantResponseDTO(final Long id, final Long productId, final String productName, final String uomMeasurement, final Integer availableQty) {
		super();
		this.id = id;
		this.productId = productId;
		this.productName = productName;
		this.measurement = uomMeasurement;
		this.availableQty = availableQty;
	}

}
