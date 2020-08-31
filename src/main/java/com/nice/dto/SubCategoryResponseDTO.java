/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Data
public class SubCategoryResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -4190427165840499007L;

	private Long id;

	private String name;

	private String nameEnglish;

	private String nameArabic;

	private Long categoryId;

	private String categoryName;

	private String image;

	private Boolean active;

}
