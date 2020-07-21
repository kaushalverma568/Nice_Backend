/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Data
public class CompanyResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6640838023686117252L;

	private Long id;

	private String name;

	private String gstin;

	private String companyEmail;

	private String customerCareEmail;

	private String companyAddress;

	private String contactNo;

	private String companyImage;

	private Boolean active;
}
