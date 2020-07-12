/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

	private List<ProductToppingDto> productToppingsDtoList;
	private List<ProductAddonsDTO> productAddonsDtoList;
	/**
	 * This Map<AttributeName, List<AttributeValues>>
	 */
	private Map<String, List<ProductAttributeValueDTO>> productAttributeValuesDtoMap;

	/**
	 * Cart Impact of Product
	 */
	private List<Long> cartQtyList = new ArrayList<>();
	private List<Long> cartIdList = new ArrayList<>();

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
