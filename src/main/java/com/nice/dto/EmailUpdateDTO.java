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
 * @date : 30-Jun-2020
 */
@Data
public class EmailUpdateDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8551594875533037479L;

	@NotBlank(message = "{email.not.null}")
	private String email;
	@NotNull(message = "{password.not.null}")
	private String password;
	@NotNull(message = "{otp.not.null}")
	private String otp;

}
