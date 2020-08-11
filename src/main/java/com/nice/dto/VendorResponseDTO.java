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

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 25, 2020
 */
@Data
public class VendorResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3419032313480089879L;

	private Long id;

	private Long businessCategoryId;

	private String businessCategoryName;

	private String email;

	private Boolean emailVerified;

	private Boolean isOrderServiceEnable;

	private String firstName;

	private String lastName;

	private String storeName;

	private String phoneNumber;

	private Long subscriptionPlanId;

	private String subscriptionPlanName;

	private Date subscriptionPlanStartDate;

	private Date subscriptionPlanEndDate;

	private String status;

	private Double minimumOrderAmt;

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

	private Long cityId;

	private String cityName;

	private Long pincodeId;

	private String codeValue;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private Double rating;

	private Long noOfRating;

	private String accepts;

	private String storeImageUrl;

	private String storeDetailImageUrl;

	private String featuredImageUrl;

	private List<VendorCuisineDTO> vendorCuisines;

	private Boolean active;

	private Double distance;

	private Boolean phoneVerified;

	private String storePhoneNumber;

	/**
	 * Consolidated Vendor address.
	 */
	private String vendorAddress;

	private Boolean isFeatured;

	private Date createdAt;

	private Boolean profileCompleted;

	private List<String> nextStatus;

}
