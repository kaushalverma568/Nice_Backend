package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Dec-2019
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class RoleDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5563304250511176544L;

	private Long id;

	@NotNull(message = "{name.not.null}")
	private String name;

	private String description;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
