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
public class CategoryWiseProductCountDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2598022841527644309L;
	private Long categoryId;
	private String categoryName;
	private Long count;

	/**
	 * @param cuisineId
	 * @param count
	 */
	public CategoryWiseProductCountDTO(final Long categoryId, final Long count) {
		super();
		this.categoryId = categoryId;
		this.count = count;
	}

}
