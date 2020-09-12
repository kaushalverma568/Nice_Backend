package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 13, 2020
 */
@Data
public class RoleAndPermissionResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7640194901940025348L;

	private Long id;

	private String name;

	private String description;

	private Boolean active;

	private Boolean isDefault;

	private List<ModuleAndPermissionResponseDTO> moduleAndPermissionResponseDTOs;
}
