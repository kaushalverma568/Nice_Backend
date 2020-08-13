package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
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
	private String image;
	private String detailImage;
	private Boolean active;
	private Date createdAt;
	private Long discountId;
	private String discountStatus;
	private List<ProductVariantResponseDTO> productVariantList;
	private List<ProductExtrasDTO> productExtrasList;
	private Double rating;
	private Long noOfRating;
	private Integer productFoodType;
	private Long cuisineId;
	private String cuisineName;
	private Boolean combo;
	/**
	 * Set to true only if all the variant of the product are out of stock
	 */
	private Boolean productAvailable;
	private Long cartQty = 0L;
}
