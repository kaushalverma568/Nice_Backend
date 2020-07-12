/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 02-07-2020
 */
@Data
public class UpdatePasswordParameterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1627745666515851438L;

	private String email;

	private String phoneNumber;
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

	/**
	 * it could be either LINK,OTP or BOTH
	 */
	private String sendingType;
}
