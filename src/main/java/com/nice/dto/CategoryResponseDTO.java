package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class CategoryResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3975990947198518915L;

	private Long id;

	private String name;

	private Boolean active;

	private String image;
}