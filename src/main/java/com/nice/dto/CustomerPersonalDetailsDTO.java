package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 5, 2020
 */
@Data
public class CustomerPersonalDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6512854416882239584L;

	@NotNull(message = "{id.not.null}")
	private Long id;

	@NotBlank(message = "{firstName.not.null}")
	private String firstName;

	@NotBlank(message = "{lastName.not.null}")
	private String lastName;

	@NotNull(message = "{birthDate.not.null}")
	private Date birthDate;
}
