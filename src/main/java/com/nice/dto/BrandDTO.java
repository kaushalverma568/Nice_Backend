package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
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
public class BrandDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3245547957010391991L;

	private Long id;

	@NotBlank(message = "{name.not.null}")
	private String name;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	/**
	 * field used to set message at the time of import
	 */
	private String uploadMessage;
}