package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class ProductAddonsDTO implements Serializable {
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

	private Double rate;

	private Long productVariantId;

	private Long vendorId;

}
