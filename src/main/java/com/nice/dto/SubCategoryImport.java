package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Data
public class SubCategoryImport implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 4040224817432223969L;

	private String nameEnglish;

	private String nameArabic;

	private String categoryNameEnglish;

	private String categoryNameArabic;

	private String uploadMessage;
}