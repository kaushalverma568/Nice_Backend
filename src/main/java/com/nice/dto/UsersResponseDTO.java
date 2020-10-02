package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class UsersResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8559207854774229436L;

	private Long id;

	private String firstName;

	private String firstNameEnglish;

	private String firstNameArabic;

	private String lastName;

	private String lastNameEnglish;

	private String lastNameArabic;

	private String email;

	private Long roleId;

	private String roleName;

	private Boolean active;

	private Long userLoginId;

	private String preferredLanguage;
}