package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Mar-2020
 * @description :
 */
@Data
public class ProductVariantRequestDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -512209078312357245L;

	private Long id;

	@NotNull(message = "{uom.id.not.null}")
	private Long uomId;

	@NotNull(message = "{rate.not.null}")
	private Double rate;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotBlank(message = "{sku.not.null}")
	private String sku;

	private Long productId;

	private Boolean productAvailable;
}
