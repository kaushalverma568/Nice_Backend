package com.nice.dto;

import java.io.Serializable;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
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
