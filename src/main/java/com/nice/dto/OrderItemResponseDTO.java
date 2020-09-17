/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Jul-2020
 */
@Data
public class OrderItemResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 5341986528575592788L;
	private Long id;
	private String productImage;
	private String productName;
	private String measurement;
	private Long orderQty;
	private Double discount;
	private Double totalAmt;
	private Double unitPrice;
	private Double unitPriceAfterDiscount;
	private Double totalDiscountAmt;
	private String productImageUrl;
	private Long productVariantId;
	private String sku;
	private String productLabel;

	/**
	 * The below field is just for display on front end, it contains the amount(all added) of order item, extras, addons,
	 * toppings and productAttributeValues
	 */
	private Double totalOrderItemAmount;
	private String categoryName;
	private List<OrderAddonsDTO> orderAddonsDtoList;
	private List<OrderToppingsDto> orderToppingsDtoList;
	private List<OrderProductAttributeValueDTO> orderProductAttributeValueDtoList;
	private List<OrderExtrasDto> orderExtraDtoList;
}
