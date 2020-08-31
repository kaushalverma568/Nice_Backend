package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Data
public class CategoryDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5856731878659821928L;

	private Long id;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotBlank(message = "{name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{name.arabic.not.null}")
	private String nameArabic;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}