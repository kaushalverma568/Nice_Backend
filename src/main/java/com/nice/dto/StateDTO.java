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
public class StateDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2705593921239990283L;

	private Long id;

	@NotBlank(message = "{name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{name.arabic.not.null}")
	private String nameArabic;

	private Long countryId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
