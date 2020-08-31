package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Data
public class CityResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7947238128297093026L;

	private Long id;

	private String name;

	private Long stateId;

	private String stateName;

	private Boolean active;

	private String nameEnglish;

	private String nameArabic;
}
