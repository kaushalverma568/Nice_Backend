package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 13, 2020
 */
@Data
public class ModuleAndPermissionResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 3021830971240509847L;

	private Long modulesId;

	private String modulesName;

	private Boolean canView;

	private Boolean canEdit;

	private Boolean canAdd;

	private Boolean canDelete;

	private Boolean sideBar;
}
