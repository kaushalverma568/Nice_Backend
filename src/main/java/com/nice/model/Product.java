/**
 *
 */
package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
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

	@Column(name = "name", nullable = false)
	private String name;

	@Column(name = "description", nullable = false)
	private String description;

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

	@Column(name = "image", nullable = false)
	private String image;

	@Column(name = "image_original_name", nullable = false)
	private String imageOriginalName;

	@Column(name = "product_available", nullable = true)
	private Boolean productAvailable;

	@Column(name = "combo", columnDefinition = "boolean default false")
	private Boolean combo;

	@Column(name = "rating")
	private Double rating;

	@Column(name = "no_of_rating")
	private Long noOfRating;
}
