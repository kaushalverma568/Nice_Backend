package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Data
public class WalletTrxDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7947238128297093026L;

	private Long id;

	@NotNull(message = "{order.id.not.null}")
	private Long orderId;

	@NotNull(message = "{customer.id.not.null}")
	private Long customerId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{amount.not.null}")
	private Double amount;

	/**
	 * for response only
	 */
	private String firstName;

	private String lastName;

}
