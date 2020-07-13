/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
@Data
public class ProductToppingDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6198999213019281393L;
	private Long id;
	@NotBlank(message = "{name.not.null}")
	private String name;
	@NotBlank(message = "{description.not.null}")
	private String description;
	private Long vendorId;
	private String productFoodType;
	@NotNull(message = "{topping.rate.not.null}")
	private Double rate;
	@NotNull(message = "{product.variant.id.not.null}")
	private Long productVariantId;
	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
