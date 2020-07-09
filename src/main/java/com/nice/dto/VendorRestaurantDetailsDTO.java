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

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 26, 2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class VendorRestaurantDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6115736981928636363L;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	@NotBlank(message = "{storeName.not.null}")
	private String storeName;

	@NotBlank(message = "{accepts.not.null}")
	private String accepts;

	@Temporal(TemporalType.TIME)
	@NotNull(message = "{openingHours.from.not.null}")
	private Date openingHoursFrom;

	@Temporal(TemporalType.TIME)
	@NotNull(message = "{openingHours.to.not.null}")
	private Date openingHoursTo;

	@NotNull(message = "{approxDeliveryTime.not.null}")
	private Integer approxDeliveryTime;

	@NotNull(message = "{minimumOrderAmt.not.null}")
	private Double minimumOrderAmt;

	@NotNull(message = "{deliveryFee.not.null}")
	private Double deliveryFee;

	@NotBlank(message = "{paymentMethod.not.null}")
	private String paymentMethod;

	@NotBlank(message = "{deliveryType.not.null}")
	private String deliveryType;

	@NotNull(message = "{cuisine.ids.not.null}")
	private List<Long> cuisineIds;

	private List<VendorCuisineDTO> vendorCuisineDTOs;

	@NotNull(message = "{isOrderServiceEnable.not.null}")
	private Boolean isOrderServiceEnable;

}
