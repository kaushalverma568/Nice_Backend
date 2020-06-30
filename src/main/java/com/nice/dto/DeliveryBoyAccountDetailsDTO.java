package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Data
public class DeliveryBoyAccountDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8383862042042618794L;

	@NotNull(message = "{id.not.null}")
	private Long id;

	@NotBlank(message = "{bank.name.not.null}")
	private String bankName;

	@NotBlank(message = "{branch.name.not.null}")
	private String branchName;

	@NotBlank(message = "{account.name.not.null}")
	private String accountName;

	@NotBlank(message = "{bank.AccountNumber.not.null}")
	private String bankAccountNumber;

	@NotBlank(message = "{kibNo.not.null}")
	private String kibNo;

	@NotBlank(message = "{branch.city.not.null}")
	private String branchCity;
}
