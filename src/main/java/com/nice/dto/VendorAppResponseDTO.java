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
 * @date   : Aug 10, 2020
 */

@Data
public class VendorAppResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5347020872407019556L;

	private Long id;
	private String storeName;
	private String storeImageUrl;
	private String storeDetailImageUrl;
	private String featuredImageUrl;
	private List<VendorCuisineDTO> vendorCuisines;
	private BigDecimal latitude;
	private BigDecimal longitude;
	private Double rating;
	private Long noOfRating;
	private Double distance;
	private Boolean isFeatured;
	private String storePhoneNumber;
	private String accepts;
	private String deliveryType;
	@Temporal(TemporalType.TIME)
	private Date openingHoursFrom;
	@Temporal(TemporalType.TIME)
	private Date openingHoursTo;
	private Double minimumOrderAmt;
	private String paymentMethod;
	private String email;
	private String preferredLanguage;
}
