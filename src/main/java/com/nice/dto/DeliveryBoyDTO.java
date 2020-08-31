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
public class DeliveryBoyDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6208884960569408293L;

	private Long id;

	@NotBlank(message = "{first.name.english.not.null}")
	private String firstNameEnglish;

	@NotBlank(message = "{last.name.english.not.null}")
	private String lastNameEnglish;

	@NotBlank(message = "{first.name.arabic.not.null}")
	private String firstNameArabic;

	@NotBlank(message = "{last.name.arabic.not.null}")
	private String lastNameArabic;

	@NotBlank(message = "{email.not.null}")
	private String email;

	@NotNull(message = "{password.required}")
	private String password;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

	/**
	 * Contains today's active time in minutes for delivery boy
	 *
	 */
	private Long activeTime;
}
