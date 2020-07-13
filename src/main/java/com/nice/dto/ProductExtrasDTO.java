package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
@ToString
public class ProductExtrasDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{name.not.null}")
	private String name;

	private String description;

	@NotNull(message = "{rate.not.null}")
	private Double rate;

	@NotNull(message = "{product.id.not.null}")
	private Long productId;

	private Long vendorId;

	/**
	 * This will be used in cart to determine the amount of quantity in cart
	 */
	private Long quantity;

}
