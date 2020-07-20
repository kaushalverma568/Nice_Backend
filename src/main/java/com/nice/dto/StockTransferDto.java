package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 28-Jan-2020
 */

@Data
public class StockTransferDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6209568810578393309L;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	private String transferedTo;
	private String transferedFrom;

	@NotNull(message = "{total.qty.not.null}")
	private Long quantity;

	@NotNull(message = "{lot.no.not.null}")
	private Long lotNo;

	@NotNull(message = "{product.id.not.null}")
	private Long productId;

	@NotNull(message = "{uom.id.not.null}")
	private Long uomId;

	private Long orderId;
	private String orderFrom;

}
