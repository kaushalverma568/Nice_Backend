package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;
import lombok.ToString;

/**
 * @author Kody Technolab PVT. LTD.
 * @date 08-Jan-2020
 */
@Data
@ToString
public class ProductParamRequestDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8156075703901624923L;

	private List<Long> categoryIds;

	private List<Long> subcategoryIds;

	private List<Long> brandIds;

	private String searchKeyword;

	private Boolean activeRecords;

	private Boolean productVariantActiveRecords;

	private Long uomId;

	private Long vendorId;
	
	private Long discountId;

	private List<Long> cuisineIds;

	private String uuid;

}
