package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

@Data
public class VendorDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6069467599398353646L;

	private Long id;

	@NotNull(message = "{business.category.id.not.null}")
	private Long businessCategoryId;

	@NotBlank(message = "{email.not.null}")
	private String email;

	private String password;

	@NotBlank(message = "{firstName.not.null}")
	private String firstName;

	@NotBlank(message = "{lastName.not.null}")
	private String lastName;

	@NotBlank(message = "{storeName.not.null}")
	private String storeName;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

	@NotBlank(message = "{building.not.null}")
	private String building;

	@NotBlank(message = "{block.not.null}")
	private String block;

	@NotBlank(message = "{street.not.null}")
	private String street;

	@NotBlank(message = "{area.not.null}")
	private String area;

	private Long countryId;

	@NotNull(message = "{city.id.not.null}")
	private Long cityId;

	@NotNull(message = "{pincode.id.not.null}")
	private Long pincodeId;

	@NotNull(message = "{latitude.not.null}")
	private BigDecimal latitude;

	@NotNull(message = "{longitude.not.null}")
	private BigDecimal longitude;

}
