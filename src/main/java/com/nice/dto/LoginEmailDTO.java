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
 * @date   : 04-Aug-2020
 */
@Data
public class LoginEmailDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1780222808931152181L;
	@NotBlank(message = "{email.not.null}")
	private String email;
	@NotNull(message = "{password.not.null}")
	private String password;

}
