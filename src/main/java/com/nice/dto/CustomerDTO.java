package com.nice.dto;

import java.io.Serializable;
import java.sql.Date;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class CustomerDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8559207854774229436L;

	private Long id;

	@NotBlank(message = "{first.name.not.null}")
	private String firstName;

	@NotBlank(message = "{last.name.not.null}")
	private String lastName;

	@NotBlank(message = "{email.not.null}")
	private String email;

	private String phoneNumber;

	private String gender;

	@NotBlank(message = "{registeredVia.name.not.null}")
	private String registeredVia;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String password;

	private Date birthDate;

	private String preferredLanguage;
}