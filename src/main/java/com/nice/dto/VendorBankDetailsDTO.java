package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 25, 2020
 */
@Data
public class VendorBankDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8383862042042618794L;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotBlank(message = "{bank.name.english.not.null}")
	private String bankNameEnglish;

	@NotBlank(message = "{bank.name.arabic.not.null}")
	private String bankNameArabic;

	@NotBlank(message = "{branch.name.english.not.null}")
	private String branchNameEnglish;

	@NotBlank(message = "{branch.name.arabic.not.null}")
	private String branchNameArabic;

	@NotBlank(message = "{account.name.english.not.null}")
	private String accountNameEnglish;

	@NotBlank(message = "{account.name.arabic.not.null}")
	private String accountNameArabic;

	@NotBlank(message = "{accountNumber.not.null}")
	private String accountNumber;

	@NotBlank(message = "{kibNo.not.null}")
	private String kibNo;

	@NotBlank(message = "{branch.city.english.not.null}")
	private String branchCityEnglish;

	@NotBlank(message = "{branch.city.arabic.not.null}")
	private String branchCityArabic;

	/**
	 * for language specific response
	 */
	private String bankName;

	private String branchName;

	private String accountName;

	private String branchCity;
}
