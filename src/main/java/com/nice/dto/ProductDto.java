/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Data
public class ProductDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8350307291002023940L;

	private Long id;
	private String name;
	private String description;
	private Long categoryId;
	private String categoryName;
	private Long subcategoryId;
	private String subCategoryName;
	private Long brandId;
	private String brandName;
	private Long cuisineId;
	private String cuisineName;
	private Long discountId;
	private String discountName;
	private Long vendorId;
	private String vendorName;
	private String image;
	private Boolean productAvailable;
	private Boolean combo;
	private List<ProductVariantDto> productVariantList;
	private Boolean active;

}
