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
public class DeliveryBoyPersonalDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7071898173592939589L;

	@NotNull(message = "{id.not.null}")
	private Long id;

	@NotBlank(message = "{firstName.not.null}")
	private String firstName;

	@NotBlank(message = "{lastName.not.null}")
	private String lastName;

	@NotBlank(message = "{email.not.null}")
	private String email;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

	@NotBlank(message = "{gender.not.null}")
	private String gender;

}
