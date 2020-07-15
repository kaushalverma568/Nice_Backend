package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 14-Jul-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class AddonsDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = 2098527942313428248L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{name.not.null}")
	private String name;

	@NotNull(message = "{description.not.null}")
	private String description;

	/**
	 * for response only
	 */
	private Long vendorId;

	private String vendorName;
}
