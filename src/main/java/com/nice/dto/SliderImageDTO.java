package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Data
public class SliderImageDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 583378005911461752L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{slider.image.type.not.null}")
	private String type;

	private String redirectUrl;

}
