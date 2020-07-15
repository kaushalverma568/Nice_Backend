/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.ToString;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
@Data
@ToString
public class ProductToppingDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6198999213019281393L;

	private Long id;

	@NotNull(message = "{topping.id.not.null}")
	private Long toppingId;

	@NotNull(message = "{topping.rate.not.null}")
	private Double rate;

	@NotNull(message = "{product.variant.id.not.null}")
	private Long productVariantId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
