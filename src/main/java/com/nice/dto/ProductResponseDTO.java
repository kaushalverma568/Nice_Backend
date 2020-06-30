package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

@Data
public class ProductResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6805990527158894688L;
	private Long id;
	private String name;
	private String description;
	private Long categoryId;
	private String categoryName;
	private Long subcategoryId;
	private String subcategoryName;
	private Long brandId;
	private String brandName;
	private Long taxId;
	private String taxName;
	private Double taxRate;
	private Long taxTypeId;
	private String taxTypeName;
	private String image;
	private String thumbnailImage;
	private Boolean active;
	private Date createdAt;
	private Long discountId;
	private String discountStatus;
	private Boolean isFeaturedProduct;
	private List<ProductVariantResponseDTO> productVariantList;
	private boolean isWishlisted;
	private Double rating;
	private Long noOfRating;
	/**
	 * Set to true only if all the variant of the product are out of stock
	 */
	private boolean productOutOfStock;
}
