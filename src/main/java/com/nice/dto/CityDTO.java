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
public class CityDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7947238128297093026L;

	private Long id;

	@NotBlank(message = "{name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{name.arabic.not.null}")
	private String nameArabic;

	private Long stateId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
