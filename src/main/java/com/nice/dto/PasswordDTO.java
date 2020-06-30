/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Data
public class PasswordDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 4027568525591628231L;

	// @NotNull(message = "{old.password.not.null}")
	private String oldPassword;
	@NotNull(message = "{new.password.not.null}")
	private String newPassword;

}
