package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class UsersDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8559207854774229436L;

	private Long id;

	@NotBlank(message = "{users.first.name.english.not.null}")
	private String firstNameEnglish;

	@NotBlank(message = "{users.last.name.english.not.null}")
	private String lastNameEnglish;

	@NotBlank(message = "{users.first.name.arabic.not.null}")
	private String firstNameArabic;

	@NotBlank(message = "{users.last.name.arabic.not.null}")
	private String lastNameArabic;

	@NotBlank(message = "{email.not.null}")
	private String email;

	@NotNull(message = "{role.id.not.null}")
	private Long roleId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String password;
}