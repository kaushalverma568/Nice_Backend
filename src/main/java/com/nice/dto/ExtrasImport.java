package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Data
public class ExtrasImport implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 69654909411313351L;

	private String nameEnglish;

	private String descriptionEnglish;

	private String nameArabic;

	private String descriptionArabic;

	private Double rate;

	private String uploadMessage;
}