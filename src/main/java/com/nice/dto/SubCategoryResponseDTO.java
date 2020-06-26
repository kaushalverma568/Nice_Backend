/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class SubCategoryResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -4190427165840499007L;

	private Long id;

	private String name;

	private Long categoryId;

	private String categoryName;

	private String image;

	private Boolean active;

}
