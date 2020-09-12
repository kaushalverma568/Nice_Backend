package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@Data
public class CartItemResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8303058211358351890L;

	private Long id;
	private Long customerId;
	private String razorPayOrderId;
	private String uuid;
	private ProductVariantResponseDTO productVariantResponseDto;
	// private List<ProductAddonsDTO> productAddonsDtoList;
	// private List<ProductToppingResponseDTO> productToppingsDtoList;
	/**
	 * This Map<AttributeName, List<AttributeValues>>
	 */
	// private Map<String, List<ProductAttributeValueDTO>> productAttributeValuesDtoMap;

	private List<ProductExtrasDTO> productExtrasDtoList;
	private Long quantity;
}