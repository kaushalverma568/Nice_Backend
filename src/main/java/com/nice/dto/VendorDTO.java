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

	@NotBlank(message = "{vendor.first.name.arabic.not.null}")
	private String firstNameArabic;
	@NotBlank(message = "{vendor.last.name.arabic.not.null}")
	private String lastNameArabic;
	@NotBlank(message = "{vendor.store.name.arabic.not.null}")
	private String storeNameArabic;
	@NotBlank(message = "{vendor.building.arabic.not.null}")
	private String buildingArabic;
	@NotBlank(message = "{vendor.block.arabic.not.null}")
	private String blockArabic;
	@NotBlank(message = "{vendor.street.arabic.not.null}")
	private String streetArabic;

	@NotBlank(message = "{vendor.first.name.english.not.null}")
	private String firstNameEnglish;
	@NotBlank(message = "{vendor.last.name.english.not.null}")
	private String lastNameEnglish;
	@NotBlank(message = "{vendor.store.name.english.not.null}")
	private String storeNameEnglish;
	@NotBlank(message = "{vendor.building.english.not.null}")
	private String buildingEnglish;
	@NotBlank(message = "{vendor.block.english.not.null}")
	private String blockEnglish;
	@NotBlank(message = "{vendor.street.english.not.null}")
	private String streetEnglish;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

	private Long countryId;

	private Long cityId;

	@NotNull(message = "{area.id.not.null}")
	private Long areaId;

	@NotNull(message = "{latitude.not.null}")
	private BigDecimal latitude;

	@NotNull(message = "{longitude.not.null}")
	private BigDecimal longitude;

	private String preferredLanguage;

	@NotNull(message = "{admin.flag.not.null}")
	private Boolean isAdmin;
}
