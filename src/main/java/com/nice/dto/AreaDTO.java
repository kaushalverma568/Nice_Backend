package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@Data
public class AreaDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 5149985441455870939L;

	private Long id;

	@NotBlank(message = "{area.name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{area.name.arabic.not.null}")
	private String nameArabic;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private Long cityId;

}
