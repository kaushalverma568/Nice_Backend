/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 26, 2020
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

	@NotBlank(message = "{openingHours.not.null}")
	private String openingHours;

	@NotBlank(message = "{approxDeliveryTime.not.null}")
	private String approxDeliveryTime;

	@NotNull(message = "{minimumOrderAmt.not.null}")
	private Double minimumOrderAmt;

	@NotNull(message = "{deliveryFee.not.null}")
	private Double deliveryFee;

	@NotBlank(message = "{paymentMethod.not.null}")
	private String paymentMethod;

	@NotBlank(message = "{deliveryType.not.null}")
	private String deliveryType;

}
