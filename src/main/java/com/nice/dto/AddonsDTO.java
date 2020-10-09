package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Jul-2020
 */
@Data
public class AddonsDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = 2098527942313428248L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{english.name.not.null}")
	private String nameEnglish;

	@NotNull(message = "{arabic.name.not.null}")
	private String nameArabic;

	private String descriptionEnglish;

	private String descriptionArabic;

	@NotNull(message = "{vendor.not.null}")
	private Long vendorId;

	private String vendorName;

	private String name;

	private String description;
}
