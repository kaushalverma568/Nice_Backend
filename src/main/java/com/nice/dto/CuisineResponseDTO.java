/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Data
public class CuisineResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 5723741674178671872L;

	private Long id;

	private String name;

	private String imageUrl;
}
