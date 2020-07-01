/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
@Data
public class CuisineWiseProductCountDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2598022841527644309L;
	private Long cuisineId;
	private String cuisineName;
	private Long count;

	/**
	 * @param cuisineId
	 * @param count
	 */
	public CuisineWiseProductCountDTO(final Long cuisineId, final Long count) {
		super();
		this.cuisineId = cuisineId;
		this.count = count;
	}

}
