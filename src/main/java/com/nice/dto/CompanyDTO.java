package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */
@Data
public class CompanyDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7062147522256197233L;

	private Long id;

	@NotBlank(message = "{company.name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{company.name.arabic.not.null}")
	private String nameArabic;

	@NotBlank(message = "{gstin.not.null}")
	private String gstin;

	@NotBlank(message = "{companyEmail.not.null}")
	private String companyEmail;

	@NotBlank(message = "{customerCareEmail.not.null}")
	private String customerCareEmail;

	@NotBlank(message = "{companyAddress.english.not.null}")
	private String companyAddressEnglish;

	@NotBlank(message = "{companyAddress.arabic.not.null}")
	private String companyAddressArabic;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String name;

	private String companyAddress;

}
