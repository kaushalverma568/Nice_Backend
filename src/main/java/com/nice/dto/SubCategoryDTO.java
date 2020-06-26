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
public class SubCategoryDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5496728237067871831L;

	private Long id;

	@NotBlank(message = "{subCategory.name.not.null}")
	private String name;

	@NotNull(message = "{category.id.not.null}")
	private Long categoryId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
