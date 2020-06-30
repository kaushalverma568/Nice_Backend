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
public class DeliveryBoyDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6208884960569408293L;

	private Long id;

	@NotBlank(message = "{fullName.not.null}")
	private String fullName;

	@NotBlank(message = "{email.not.null}")
	private String email;

	@NotNull(message = "{password.required}")
	private String password;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

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
