package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotNull;

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

	@NotNull(message = "{business.category.id.not.null}")
	private Long businessCategoryId;

	private String deliveryType;

	private List<Long> cuisineIds;

	private String searchKeyword;

	private Long customerAddressId;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private List<Long> vendorIds;

	@Temporal(TemporalType.TIME)
	private Date openingHours;

	private Boolean isFeatured;

	private Double ratingTo;

	private Double ratingFrom;
}
