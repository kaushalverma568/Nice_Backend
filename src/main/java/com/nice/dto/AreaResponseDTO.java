package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@Data
public class AreaResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8812368658987078781L;

	private Long id;

	private String name;

	private String nameEnglish;

	private String nameArabic;

	private Boolean active;

	private Long cityId;

	private String cityName;

	private String cityNameEnglish;

	private String cityNameArabic;
}
