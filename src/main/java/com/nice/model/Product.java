/**
 *
 */
package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityResult;
import javax.persistence.FieldResult;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@SqlResultSetMapping(name = "ProductMapping", entities = { @EntityResult(entityClass = Product.class, fields = { @FieldResult(name = "id", column = "id"),
		@FieldResult(name = "active", column = "active"), @FieldResult(name = "createdAt", column = "created_at"),
		@FieldResult(name = "updatedAt", column = "updated_at"), @FieldResult(name = "createdBy", column = "created_by"),
		@FieldResult(name = "updatedBy", column = "updated_by"), @FieldResult(name = "nameEnglish", column = "name_english"),
		@FieldResult(name = "nameArabic", column = "name_arabic"), @FieldResult(name = "descriptionEnglish", column = "description_english"),
		@FieldResult(name = "descriptionArabic", column = "description_arabic"), @FieldResult(name = "imageOriginalName", column = "image_original_name"),
		@FieldResult(name = "categoryId", column = "category_id"), @FieldResult(name = "discountId", column = "discount_id"),
		@FieldResult(name = "subcategoryId", column = "subcategory_id"), @FieldResult(name = "brandId", column = "brand_id"),
		@FieldResult(name = "image", column = "image"), @FieldResult(name = "productAvailable", column = "product_available"),
		@FieldResult(name = "vendorId", column = "vendor_id"), @FieldResult(name = "cuisineId", column = "cuisine_id"),
		@FieldResult(name = "combo", column = "combo"), @FieldResult(name = "rating", column = "rating"),
		@FieldResult(name = "noOfRating", column = "no_of_rating"), @FieldResult(name = "detailImage", column = "detail_image"),
		@FieldResult(name = "detailImageOriginalName", column = "detail_image_original_name"),
		@FieldResult(name = "productFoodType", column = "product_food_type"), @FieldResult(name = "businessCategoryId", column = "business_category_id") }) })

@Data
@Entity(name = "product")
@Table(name = "product")
@EqualsAndHashCode(callSuper = false)
public class Product extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = 355492703180332337L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false)
	private String nameEnglish;

	@Column(name = "description_english", nullable = false)
	private String descriptionEnglish;

	@Column(name = "name_arabic", nullable = false)
	private String nameArabic;

	@Column(name = "description_arabic", nullable = false)
	private String descriptionArabic;

	@Column(name = "category_id", nullable = false)
	private Long categoryId;

	@Column(name = "subcategory_id", nullable = true)
	private Long subcategoryId;

	@Column(name = "brand_id", nullable = true)
	private Long brandId;

	@Column(name = "cuisine_id", nullable = true)
	private Long cuisineId;

	@Column(name = "discount_id")
	private Long discountId;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "image")
	private String image;

	@Column(name = "image_original_name")
	private String imageOriginalName;

	@Column(name = "detail_image")
	private String detailImage;

	@Column(name = "detail_image_original_name")
	private String detailImageOriginalName;

	@Column(name = "product_available", nullable = true)
	private Boolean productAvailable;

	@Column(name = "combo", columnDefinition = "boolean default false")
	private Boolean combo;

	@Column(name = "rating")
	private Double rating;

	@Column(name = "no_of_rating")
	private Long noOfRating;

	@Column(name = "product_food_type")
	private Integer productFoodType;

	@Column(name = "business_category_id")
	private Long businessCategoryId;
}
