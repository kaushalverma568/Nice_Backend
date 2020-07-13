/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Jul-2020
 */
@Data
@EqualsAndHashCode
public class OrderItemResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 5341986528575592788L;
	private Long id;
	private String productImage;
	private String productName;
	private String uomLabel;
	private Long orderQty;
	private Double discount;
	private Double totalAmt;
	private Double unitPrice;
	private Double unitPriceAfterDiscount;
	private Double totalDiscountAmt;
	private Long replaceQty;
	private String productImageUrl;
	private Long productVariantId;
	private String categoryName;
	private List<OrderAddonsDTO> orderAddonsDtoList;
	private List<OrderToppingsDto> orderToppingsDtoList;
	private List<OrderProductAttributeValueDTO> orderProductAttributeValueDtoList;
	private List<OrderExtrasDto> orderExtraDtoList;
}
