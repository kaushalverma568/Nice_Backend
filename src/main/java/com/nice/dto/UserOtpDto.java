package com.nice.dto;

import javax.validation.constraints.NotBlank;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Data
public class UserOtpDto {

	private Long id;
	private Long userId;
	@NotBlank(message = "{otp.type.not.null}")
	private String type;
	private String email;
	private String userType;
	private String otp;
	private String phoneNumber;

}
