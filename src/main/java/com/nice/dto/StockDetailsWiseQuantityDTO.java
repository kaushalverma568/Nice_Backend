/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Data
@EqualsAndHashCode
public class StockDetailsWiseQuantityDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1740809658484395539L;

	@NotNull(message = "{productVariant.id.not.null}")
	private Long productVariantId;

	@NotNull(message = "{quantity.not.null}")
	@Min(value = 1, message = "{quantity.non.zero}")
	private Long quantity;

	@NotNull(message = "{order.item.not.null}")
	private Long orderItemId;

	@NotNull(message = "{lot.no.not.null}")
	private Long lotNo;
}
