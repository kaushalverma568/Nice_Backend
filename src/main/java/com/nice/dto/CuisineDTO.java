package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Data
public class CuisineDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2806238291882677511L;

	private Long id;

	@NotNull(message = "{name.not.null}")
	private String name;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
