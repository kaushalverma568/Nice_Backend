package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 7, 2020
 */
@Data
public class RoleAndPermissionsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -741896060295988037L;

	private Long id;

	@NotNull(message = "{name.not.null}")
	private String name;

	private String description;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private List<ModuleAndPermissionDTO> moduleAndPermissionDTOs;
}
