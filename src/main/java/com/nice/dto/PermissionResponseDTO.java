package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class PermissionResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 3408965874290213790L;

	private Long id;

	private Long roleId;

	private String roleName;

	private Long moduleId;

	private String moduleName;

	private Long userId;

	private Boolean canView;

	private Boolean canEdit;

	private Boolean canAdd;

	private Boolean canDelete;

	private Boolean canImport;

	private Boolean canExport;

	private Boolean active;

}
