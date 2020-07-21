/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 01-Jul-2020
 */
@Data
public class ProductToppingResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3599858591528245080L;

	private Long id;

	private Long toppingId;

	private Double rate;

	private Long vendorId;

	private Long productVariantId;

	private Boolean active;

	private String name;

	private String description;

	private String productFoodType;

}
