package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class PincodeResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2621751737193285642L;

	private Long id;

	private String codeValue;

	private Long cityId;

	private String cityName;

	private Boolean active;

	private Boolean isDefault;
}
