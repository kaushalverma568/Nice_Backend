package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Data
public class DeliveryBoyPersonalDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7071898173592939589L;

	@NotNull(message = "{id.not.null}")
	private Long id;

	@NotBlank(message = "{first.name.english.not.null}")
	private String firstNameEnglish;

	@NotBlank(message = "{last.name.english.not.null}")
	private String lastNameEnglish;

	@NotBlank(message = "{first.name.arabic.not.null}")
	private String firstNameArabic;

	@NotBlank(message = "{last.name.arabic.not.null}")
	private String lastNameArabic;

	@NotBlank(message = "{gender.not.null}")
	private String gender;

}
