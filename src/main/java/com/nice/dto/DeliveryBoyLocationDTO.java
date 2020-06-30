package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 19, 2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoyLocationDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2897849519256165040L;

	private Long id;

	@NotNull(message = "{deliveryboy.id.not.null}")
	private Long deliveryBoyId;

	@NotNull(message = "{latitude.not.null}")
	private BigDecimal latitude;

	@NotNull(message = "{longitude.not.null}")
	private BigDecimal longitude;

}
