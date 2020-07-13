/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 25, 2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class VendorResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3419032313480089879L;

	private Long id;

	private Long businessCategoryId;

	private String businessCategoryName;

	private String email;

	private Boolean isEmailVerified;

	private Boolean isOrderServiceEnable;

	private String firstName;

	private String lastName;

	private String storeName;

	private String contactNo;

	private Long subscriptionPlanId;

	private String subscriptionPlanName;

	private Date subscriptionPlanStartDate;

	private Date subscriptionPlanEndDate;

	private String status;

	private Integer approxDeliveryTime;

	private Double minimumOrderAmt;

	private Double deliveryFee;

	private String paymentMethod;

	private String building;

	private String block;

	private String street;

	private String area;

	private String deliveryType;

	@Temporal(TemporalType.TIME)
	private Date openingHoursFrom;

	@Temporal(TemporalType.TIME)
	private Date openingHoursTo;

	private Long countryId;

	private String countryName;

	private Long cityId;

	private String cityName;

	private Long pincodeId;

	private String codeValue;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private Double rating;

	private Long noOfRating;

	private String accepts;

	private String profilePictureUrl;

	private List<VendorCuisineDTO> vendorCuisines;

	private Boolean active;

	private Double distance;

	private Boolean isContactVerified;
}
