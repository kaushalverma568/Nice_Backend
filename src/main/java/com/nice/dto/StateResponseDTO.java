/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class StateResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1369431531718004344L;

	private Long id;

	private String name;

	private String nameEnglish;

	private String nameArabic;

	private Long countryId;

	private String countryName;

	private String countryNameEnglish;

	private String countryNameArabic;

	private Boolean active;
}
