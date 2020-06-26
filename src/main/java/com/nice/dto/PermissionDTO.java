package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class PermissionDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 328059274224598196L;

	private Long id;

	@NotNull(message = "{role.id.not.null}")
	private Long roleId;

	@NotNull(message = "{modules.id.not.null}")
	private Long modulesId;

	private Boolean canView;

	private Boolean canViewList;

	private Boolean canEdit;

	private Boolean canAdd;

	private Boolean canDelete;

	private Boolean canImport;

	private Boolean canExport;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
