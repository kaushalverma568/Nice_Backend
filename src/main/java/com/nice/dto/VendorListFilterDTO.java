package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.springframework.format.annotation.DateTimeFormat;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Data
public class VendorListFilterDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -9104413848520087852L;

	private Long businessCategoryId;

	private String deliveryType;

	private List<Long> cuisineIds;

	private String searchKeyword;

	private Long customerAddressId;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private List<Long> vendorIds;

	@Temporal(TemporalType.TIME)
	@DateTimeFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ")
	private Date openingHours;

	private Boolean isFeatured;

	private Double ratingTo;

	private Double ratingFrom;

	private Boolean isNameSorting;

	private Double distance;

	private Long cityId;

	private Boolean isPopular;

	private Boolean isNewArrival;

}
