package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

@Data
public class SideBarDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5407715141796805906L;

	private Long modulesId;

	private String modulesName;

	private Boolean canView;

	private Boolean canEdit;

	private Boolean canAdd;

	private Boolean canDelete;

	private List<ModuleAndPermissionResponseDTO> children;

}
