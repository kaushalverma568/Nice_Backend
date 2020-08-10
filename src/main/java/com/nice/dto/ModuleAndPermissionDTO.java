package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 7, 2020
 */
@Data
public class ModuleAndPermissionDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3113943464606449820L;

	@NotNull(message = "{modules.id.not.null}")
	private Long modulesId;

	private Boolean canView;

	private Boolean canViewList;

	private Boolean canEdit;

	private Boolean canAdd;

	private Boolean canDelete;

	private Boolean canImport;

	private Boolean canExport;
}
