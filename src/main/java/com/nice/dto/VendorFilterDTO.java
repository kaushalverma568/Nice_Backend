package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
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
}
