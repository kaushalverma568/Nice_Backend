/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 18-Sep-2020
 */
@Data
public class RefundAmountDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8006461314873702528L;

	@NotNull(message = "{order.is.required}")
	private Long orderId;
	private String description;

	/**
	 * This fields should be used in if in future we need to divide the customer repaid amount between admin, delivery boy
	 * and vendor. So that admin doesn't have to bear the entire amount for the cancelled order</br>
	 * Who will bear how much amount will be decided by the admin
	 */
	@NotNull(message = "{amount.is.required}")
	private Double adminContribution = 0d;
	private Double vendorContribution = 0d;
	private Double deliveryBoyContribution = 0d;
}
