package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 20-07-2020
 */
@Data
public class ProductImportDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -4709686418979806851L;

	private String nameEnglish;

	private String nameArabic;

	private String descriptionEnglish;

	private String descriptionArabic;

	private String categoryNameEnglish;

	private String categoryNameArabic;

	private String subcategoryNameEnglish;

	private String subcategoryNameArabic;

	private Boolean combo;

	private String brandNameEnglish;

	private String brandNameArabic;

	private Boolean productAvailable;

	private String cuisineNameEnglish;

	private String cuisineNameArabic;

	/**
	 * this need to be either Veg , Non_veg or egg (case in-sensitive)
	 */
	private String productFoodType;

	private Long vendorId;

	private String uploadMessage;
}
