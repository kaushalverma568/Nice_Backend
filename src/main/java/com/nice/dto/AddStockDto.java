package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jan-2020
 */

@Data
public class AddStockDto implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 7200203625064820144L;

	@NotNull(message = "{product.id.not.null}")
	private Long productId;
	@NotNull(message = "{uom.id.not.null}")
	private Long uomId;
	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;
	@NotNull(message = "{update.stock.not.null}")
	private Long totalQty;
	@NotNull(message = "{lot.no.not.null}")
	private Long lotNo;
	@NotNull(message = "{expiryDate.not.null}")
	private Date expiryDate;
	@NotNull(message = "{lotDate.not.null}")
	private Date lotDate;

}
