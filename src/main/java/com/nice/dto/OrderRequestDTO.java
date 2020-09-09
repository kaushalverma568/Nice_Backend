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
 * @date : 07-Jul-2020
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

	/**
	 * This flag will be used to indicate if the wallet needs to be used for this order or not. This flag if true can have
	 * following two cases: 1. Entire order amount would be settled from wallet balance. 2. Partial order amount is settled
	 * via wallet and other amount would either be COD or online payment depending upon the payment mode selected
	 */
	private Boolean useWallet = false;

	private String transactionId;

	private String onlineOrderId;

	private String onlineSignature;

	private String firstName;

	private String lastName;

	private String addressEnglish;

	private String addressArabic;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private Long stateId;

	private Long cityId;

	private Long pincodeId;

	private String phoneNumber;

	private Long vendorId;

	/**
	 * special request for the order
	 */
	private String description;

	/**
	 * Wallet contribution will be used internally
	 */
	private Double walletContribution;
}
