package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */

@Data
public class ProductAttributeDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{english.name.not.null}")
	private String nameEnglish;

	@NotNull(message = "{arabic.name.not.null}")
	private String nameArabic;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String descriptionEnglish;

	private String descriptionArabic;

	private Long vendorId;

	/**
	 * for response only
	 */
	private String vendorName;

	private String name;

	private String description;
}
