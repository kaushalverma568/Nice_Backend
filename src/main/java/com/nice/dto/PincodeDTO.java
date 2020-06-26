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
public class PincodeDTO implements Serializable {

	/**
	*
	*/
	private static final long serialVersionUID = -3040740414645046560L;

	private Long id;

	@NotBlank(message = "{codeValue.not.null}")
	private String codeValue;

	@NotNull(message = "{city.id.not.null}")
	private Long cityId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
