/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Data
public class ProductVariantDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7682658129755364646L;

	private Long id;
	private Long productId;
	private String productName;
	private Double rate;
	private Double discountedRate;
	private Long vendorId;
	private Boolean productAvailable;
	private String sku;
	private Boolean active;
	/**
	 * Need for Grocery, to show product out of stock if stock not available
	 */
	private Long availableQty;
}
