/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class UserLoginDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7133474062798438886L;
	@NotBlank(message = "{username.not.null}")
	private String userName;
	@NotNull(message = "{password.not.null}")
	private String password;

	/**
	 * CUSTOMER,DELIVERY_BOY,ADMIN,USER
	 */
	private String userType;

	/**
	 * For Login purpose only : OTP,APP,GOOGLE,FACEBOOK
	 */
	private String registeredVia;

	/**
	 * For Email purpose only
	 */
	private boolean isNewCustomer;
	private Long userId;
}
