package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Jul-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class OrderLocationDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2897849519256165040L;

	private Long id;

	@NotNull(message = "{deliveryboy.id.not.null}")
	private Long deliveryBoyId;

	@NotNull(message = "{order.id.not.null}")
	private Long orderId;

	@NotNull(message = "{customer.id.not.null}")
	private Long customerId;

	@NotNull(message = "{latitude.not.null}")
	private BigDecimal latitude;

	@NotNull(message = "{longitude.not.null}")
	private BigDecimal longitude;

}
