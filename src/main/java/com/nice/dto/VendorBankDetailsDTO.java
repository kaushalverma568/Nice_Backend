package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 25, 2020
 */
@Data
public class VendorBankDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8383862042042618794L;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotBlank(message = "{bank.name.not.null}")
	private String bankName;

	@NotBlank(message = "{branch.name.not.null}")
	private String branchName;

	@NotBlank(message = "{account.name.not.null}")
	private String accountName;

	@NotBlank(message = "{accountNumber.not.null}")
	private String accountNumber;

	@NotBlank(message = "{kibNo.not.null}")
	private String kibNo;

	@NotBlank(message = "{branch.city.not.null}")
	private String branchCity;
}
