package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 31, 2020
 */
@Data
public class SliderImageResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1542384865184629059L;

	private Long id;

	private String imageUrl;

	private String imageEnglishUrl;

	private String imageArabicUrl;

	private Boolean active;

	private String type;

	private String redirectUrl;

}
