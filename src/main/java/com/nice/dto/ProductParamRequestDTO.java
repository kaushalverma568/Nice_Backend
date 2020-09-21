package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author Kody Technolab PVT. LTD.
 * @date 08-Jan-2020
 */
@Data
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

	private List<Long> businessCategoryIds;

	private List<Long> exceptBusinessCategoryIds;

	private Integer productFoodType;

	/**
	 * Used internally: If the request if from admin, then we need to display all the product even if they have no variants
	 * associated with them, for customer requests such products should not be displayed as rates and UOM would not be
	 * displayed for the same
	 */
	private boolean isFromAdmin;
}
