package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */

@Data
public class ProductAttributeValueDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotEmpty(message = "{english.attribute.value.not.null}")
	private String attributeValueEnglish;

	@NotEmpty(message = "{arabic.attribute.value.not.null}")
	private String attributeValueArabic;

	private String descriptionEnglish;

	private String descriptionArabic;

	private Double rate;

	private Long productVariantId;

	private Long productAttributeId;

	private String productAttributeName;

	/**
	 * This will be used only for response, it will contain the language specific values for the fields.
	 */
	private String attributeValue;

	private String description;

}
