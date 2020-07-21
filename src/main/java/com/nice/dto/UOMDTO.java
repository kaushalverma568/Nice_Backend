package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Data
public class UOMDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -641864522538012591L;

	private Long id;

	@NotBlank(message = "{uom.measurement.not.null}")
	private String measurement;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{uom.quantity.not.null}")
	private Double quantity;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	/**
	 * for response purpose only
	 */
	private String uomLabel;

}