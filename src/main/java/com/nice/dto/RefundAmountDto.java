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
	@NotNull(message = "{amount.is.required}")
	private Double amount;
	@NotNull(message = "{order.is.required}")
	private Long orderId;
	private String description;
}
