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

	private String descriptionEnglish;

	private String nameArabic;

	private String descriptionArabic;

	private String categoryNameEnglish;

	private String subcategoryNameEnglish;

	private String categoryNameArabic;

	private String subcategoryNameArabic;

	private String brandName;

	private Long vendorId;

	private String cuisineName;

	private Boolean productAvailable;

	private Boolean combo;

	/**
	 * this need to be either Veg , Non_veg or egg (case in-sensitive)
	 */
	private String productFoodType;

	private String uploadMessage;
}
