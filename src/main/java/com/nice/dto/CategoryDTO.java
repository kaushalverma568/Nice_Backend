package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class CategoryDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5856731878659821928L;

	private Long id;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotBlank(message = "{name.not.null}")
	private String name;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}