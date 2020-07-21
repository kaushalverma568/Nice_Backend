
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class SettingsDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2133542493619752171L;
	private Long id;
	@NotBlank(message = "{field.name.not.empty}")
	private String fieldName;
	@NotBlank(message = "{field.value.not.empty}")
	private String fieldValue;
	@NotNull(message = "{specify.field.encrypted}")
	private Boolean encrypted;
	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
