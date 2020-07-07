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
public class ProductAttributeValueDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String attributeValue;

	private String description;

	private Double rate;

	private Long productVariantId;

	private Long productAttributeId;

	/**
	 * for response only
	 */
	private String productName;

	private String productAttributeName;
}
