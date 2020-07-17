package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Data
public class AddonsImport implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 69654909411313351L;
	
	private String name;
	
	private String description;

	private String uploadMessage;
}