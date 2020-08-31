package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class ProductExtrasMasterDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String name;

	private String description;

	@NotBlank(message = "{english.name.not.null}")
	private String nameEnglish;

	private String descriptionEnglish;

	@NotBlank(message = "{arabic.name.not.null}")
	private String nameArabic;

	private String descriptionArabic;

	@NotNull(message = "{rate.not.null}")
	private Double rate;

	private Long vendorId;

}
