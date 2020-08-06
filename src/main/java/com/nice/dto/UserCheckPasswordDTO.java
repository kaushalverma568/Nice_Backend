package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Aug 05, 2020
 */
@Data
public class UserCheckPasswordDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 5050371211899938300L;

	@NotNull(message = "{entity.id.not.null}")
	private Long entityId;
	@NotNull(message = "{entity.type.not.null}")
	private String entityType;
	@NotNull(message = "{password.not.null}")
	private String password;

}
