package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Mar-2020
 * @description :
 */
@Data
public class ProductRequestDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 3626158150713137105L;

	private Long id;

	@NotBlank(message = "{product.name.not.null}")
	private String name;

	private String description;

	@NotNull(message = "{category.id.not.null}")
	private Long categoryId;

	@NotNull(message = "{subcategory.id.not.null}")
	private Long subcategoryId;

	@NotNull(message = "{brand.id.not.null}")
	private Long brandId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotNull(message = "{cuisine.id.not.null}")
	private Long cuisineId;

	private Boolean productAvailable;

	private Boolean combo;

	@NotNull(message = "{food.type.not.null}")
	private String productFoodType;

}
