package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class ModulesDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -538213788252365937L;

	private Long id;

	@NotBlank(message = "{name.not.null}")
	private String name;

	@NotBlank(message = "{userRole.not.null}")
	private String userRole;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
