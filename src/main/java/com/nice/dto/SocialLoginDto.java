package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class SocialLoginDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6091943085751628893L;

	@NotBlank(message = "{first.name.not.null}")
	private String firstName;

	@NotBlank(message = "{last.name.not.null}")
	private String lastName;

	@NotBlank(message = "{email.not.null}")
	private String email;

	private String gender;

	@NotBlank(message = "{unique.id.not.null}")
	private String uniqueId;

	@NotBlank(message = "{registeredVia.name.not.null}")
	private String registeredVia;

	private String clientId;

	private String clientSecret;

	private Long userId;

	/**
	 * For email trigger only
	 */
	private boolean isNewCustomer;

	/**
	 * For email purpose only
	 */
	private Long customerId;

}
