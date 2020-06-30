package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class CompanyDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7062147522256197233L;

	private Long id;

	@NotBlank(message = "{company.name.not.null}")
	private String name;

	@NotBlank(message = "{gstin.not.null}")
	private String gstin;

	@NotBlank(message = "{companyEmail.not.null}")
	private String companyEmail;

	@NotBlank(message = "{customerCareEmail.not.null}")
	private String customerCareEmail;

	@NotBlank(message = "{companyAddress.not.null}")
	private String companyAddress;

	@NotBlank(message = "{contactNo.not.null}")
	private String contactNo;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
