package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.ToString;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 28-Jan-2020
 */
@Data
@ToString
public class StockDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2708383507628139175L;

	@NotNull(message = "{batchNo.not.null}")
	private Long batchNo;

	@NotNull(message = "{vendorId.not.null}")
	private Long vendorId;

	@NotNull(message = "{productVariantId.not.null}")
	private Long productVariantId;

	@NotNull(message = "{quantity.not.null}")
	private Long quantity;

	@NotNull(message = "{expiryDate.not.null}")
	private Date expiryDate;

}
