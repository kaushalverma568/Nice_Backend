package com.nice.dto;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Data
public class SliderImageDTO {
	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String imageUrl;

	@NotNull(message = "{slider.image.type.not.null}")
	private String type;
}
