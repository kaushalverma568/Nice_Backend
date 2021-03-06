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
	private String nameEnglish;
	private String nameArabic;
	private String description;
	private String descriptionArabic;
	private String descriptionEnglish;
	private Long categoryId;
	private String categoryName;
	private String categoryNameEnglish;
	private String categoryNameArabic;
	private Long subcategoryId;
	private String subcategoryName;
	private String subcategoryNameArabic;
	private String subcategoryNameEnglish;
	private Long brandId;
	private String brandName;
	private String brandNameEnglish;
	private String brandNameArabic;
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
	private String cuisineNameEnglish;
	private String cuisineNameArabic;
	private Boolean combo;
	/**
	 * Set to true only if atleast one variant of product exists
	 */
	private Boolean productAvailable;
	/**
	 * This will be set to true if product is out of stock and only for food delivery business category for others this will
	 * be set to false
	 */
	private Boolean productOutOfStock;
	private Long cartQty = 0L;
	private String businessCategoryName;
	private String businessCategoryNameArabic;
	private String businessCategoryNameEnglish;
	private Long businessCategoryId;
	private String storeName;
	private String storeNameEnglish;
	private String storeNameArabic;
	private String storeContactNumber;
	private Boolean manageInventory;
}
