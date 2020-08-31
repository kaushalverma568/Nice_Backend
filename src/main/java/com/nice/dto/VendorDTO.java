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

	@NotBlank(message = "{firstName.arabic.not.null}")
	private String firstNameArabic;
	@NotBlank(message = "{lastName.arabic.not.null}")
	private String lastNameArabic;
	@NotBlank(message = "{storeName.arabic.not.null}")
	private String storeNameArabic;
	@NotBlank(message = "{building.arabic.not.null}")
	private String buildingArabic;
	@NotBlank(message = "{block.arabic.not.null}")
	private String blockArabic;
	@NotBlank(message = "{street.arabic.not.null}")
	private String streetArabic;
	@NotBlank(message = "{area.arabic.not.null}")
	private String areaArabic;

	@NotBlank(message = "{firstName.english.not.null}")
	private String firstNameEnglish;
	@NotBlank(message = "{lastName.english.not.null}")
	private String lastNameEnglish;
	@NotBlank(message = "{storeName.english.not.null}")
	private String storeNameEnglish;
	@NotBlank(message = "{building.english.not.null}")
	private String buildingEnglish;
	@NotBlank(message = "{block.english.not.null}")
	private String blockEnglish;
	@NotBlank(message = "{street.english.not.null}")
	private String streetEnglish;
	@NotBlank(message = "{area.english.not.null}")
	private String areaEnglish;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

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
