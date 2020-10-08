package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class VendorFilterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7992154691907146552L;

	private Long businessCategoryId;

	private Long countryId;

	private Long cityId;

	private Long pincodeId;

	private String searchKeyword;

	private Boolean activeRecords;

	private Date subscriptionEndDate;

	private Date subscriptionEndDateLessthanEqual;

	private Boolean isFeatured;

	private String status;

	private Long subscriptionPlanId;

	private String sortByDirection;

	private String sortByField;

	private Boolean isProfileCompleted;
}
