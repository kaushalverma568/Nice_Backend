/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;

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

	private String nameEnglish;

	private String nameArabic;

	private String gstin;

	private String companyEmail;

	private String customerCareEmail;

	private String companyAddress;

	private String companyAddressEnglish;

	private String companyAddressArabic;

	private String phoneNumber;

	private String companyImage;

	private Boolean active;

	private BigDecimal latitude;

	private BigDecimal longitude;
}
