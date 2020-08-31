package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */
@Data
public class UOMDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -641864522538012591L;

	private Long id;

	@NotBlank(message = "{measurement.english.not.null}")
	private String measurementEnglish;

	@NotBlank(message = "{measurement.arabic.not.null}")
	private String measurementArabic;

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
	private String measurement;
	private String uomLabelEnglish;
	private String uomLabelArabic;

}