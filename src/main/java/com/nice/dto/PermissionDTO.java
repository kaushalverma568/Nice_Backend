package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
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

	private Boolean canEdit;

	private Boolean canAdd;

	private Boolean canDelete;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
