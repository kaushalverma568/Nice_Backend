package com.nice.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class LoginResponse implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -9129942395904924485L;

	@JsonProperty("access_token")
	private String accessToken;

	@JsonProperty("token_type")
	private String tokenType;

	@JsonProperty("refresh_token")
	private String refreshToken;

	@JsonProperty("expires_in")
	private long expiresIn;

	private String scope;

	private Long userId;

	private String firstName;

	private String firstNameEnglish;

	private String firstNameArabic;
	private String lastName;

	private String lastNameEnglish;

	private String lastNameArabic;

	private String message;

	private int status;

	private Long entityId;

	private String entityType;

	private String email;

	private String phoneNumber;

	private Long roleId;

	private String roleName;

	private Boolean canChangePassword;

	private String preferredLanguage;

	private Boolean emailVerified;

	private Boolean phoneVerified;
}
