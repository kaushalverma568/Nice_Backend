package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Data
public class CuisineDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2806238291882677511L;

	private Long id;

	@NotBlank(message = "{name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{name.arabic.not.null}")
	private String nameArabic;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
