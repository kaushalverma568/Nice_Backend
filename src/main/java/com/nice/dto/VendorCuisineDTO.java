package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 01-Jul-2020
 */
@Data
public class VendorCuisineDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7164555967749447203L;

	private Long id;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;
	@NotNull(message = "{cuisine.id.not.null}")
	private Long cuisineId;
	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private String vendorName;
	private String cuisineName;
	private String storeName;
}
