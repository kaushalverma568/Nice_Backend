/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 07-Jul-2020
 */
@Data
public class OrderRequestDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 3284884077855088331L;

	private Long customerId;

	@NotNull(message = "{shipping.id.required}")
	private Long shippingAddressId;

	@NotNull(message = "{total.amount.required}")
	private Double totalOrderAmount;

	@NotBlank(message = "{payment.mode.required}")
	private String paymentMode;
	/**
	 * this will be either Pick-Up or Delivery
	 */
	@NotBlank(message = "{delivery.type.required}")
	private String deliveryType;

	private String transactionId;

	private String onlineOrderId;

	private String onlineSignature;

	private String firstName;

	private String lastName;

	private String address;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private Long stateId;

	private Long cityId;

	private Long pincodeId;

	private String phoneNumber;

	private Long vendorId;

	/**
	 * Added For coupon code
	 */
	private String couponCode;

}
