/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Jul-2020
 */
@Data
@EqualsAndHashCode
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

	private String transactionId;

	private String onlineOrderId;

	private String onlineSignature;

	private String firstName;

	private String lastName;

	private String address;

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
