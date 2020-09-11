/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotBlank;
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
public class StockAllocationDto implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = 2862088809666514849L;

	private Long id;

	private Long deliveryBoyId;

	@NotNull(message = "{order.id.not.null}")
	private Long orderId;

	private Long vendorId;

	private List<StockDetailsWiseQuantityDTO> stockDetailsWiseQtyList;
	/**
	 * This would contain if the stock is allocated for delivery or replacement.
	 */
	@NotBlank(message = "{allocated.for.required}")
	private String allocatedFor;

}
