/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 02-07-2020
 */
@Data
public class ResetPasswordParameterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8246787142915451851L;

	@NotBlank(message = "{email.not.null}")
	private String email;

	@NotBlank(message = "{otp.not.null}")
	private String otp;
	/**
	 * It could be either customer,user or super_admin
	 */
	@NotBlank(message = "{user.type.not.null}")
	private String userType;

	/**
	 * it could be either SMS or EMAIL
	 */
	@NotBlank(message = "{type.not.null}")
	private String type;

	@NotBlank(message = "{password.not.null}")
	private String password;
}