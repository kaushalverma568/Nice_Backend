package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Data
public class BrandImport implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7966696376518001219L;

	private String nameEnglish;

	private String nameArabic;

	private String uploadMessage;
}