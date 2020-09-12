/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import org.springframework.format.annotation.DateTimeFormat;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 26, 2020
 */
@Data
public class VendorRestaurantDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6115736981928636363L;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotBlank(message = "{vendor.store.name.english.not.null}")
	private String storeNameEnglish;

	@NotBlank(message = "{vendor.store.name.arabic.not.null}")
	private String storeNameArabic;

	@NotBlank(message = "{accepts.not.null}")
	private String accepts;

	@Temporal(TemporalType.TIME)
	@DateTimeFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ")
	@NotNull(message = "{openingHours.from.not.null}")
	private Date openingHoursFrom;

	@Temporal(TemporalType.TIME)
	@DateTimeFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ")
	@NotNull(message = "{openingHours.to.not.null}")
	private Date openingHoursTo;

	@NotNull(message = "{minimumOrderAmt.not.null}")
	private Double minimumOrderAmt;

	@NotBlank(message = "{paymentMethod.not.null}")
	private String paymentMethod;

	@NotBlank(message = "{deliveryType.not.null}")
	private String deliveryType;

	@NotNull(message = "{cuisine.ids.not.null}")
	private List<Long> cuisineIds;

	private List<VendorCuisineDTO> vendorCuisineDTOs;

	@NotBlank(message = "{store.contact.not.null}")
	private String storePhoneNumber;

	/**
	 * maximum time for which vendor can accept return/replace (in days)
	 */
	@NotNull(message = "{maxDaysForAccept.not.null}")
	private Integer maxDaysForAccept;

	@NotNull(message = "{isOrderServiceEnable.not.null}")
	private Boolean isOrderServiceEnable;
}
