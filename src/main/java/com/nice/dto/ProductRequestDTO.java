package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 10-Jul-2020
 */
@Data
public class ProductRequestDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 3626158150713137105L;

	private Long id;

	@NotBlank(message = "{name.not.null}")
	private String name;

	@NotBlank(message = "{description.not.null}")
	private String description;

	@NotNull(message = "{category.id.not.null}")
	private Long categoryId;

	private Long subcategoryId;

	private Long brandId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	private Long cuisineId;

	private Boolean productAvailable;

	private Boolean combo;

	@NotNull(message = "{food.type.not.null}")
	private Integer productFoodType;

}
