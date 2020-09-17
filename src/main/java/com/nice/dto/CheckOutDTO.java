/**
 *
 */
package com.nice.dto;

import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Sep-2020
 */
@Data
public class CheckOutDTO {

	private List<CartItemResponseDTO> cartItemResponseList;

	/**
	 * Mapped to Subtotal
	 */
	private Double grossOrderAmount;

	/**
	 * Mapped to deliveryCharge
	 */
	private Double deliveryCharge;

	/**
	 * Mapped to totalAmount
	 */
	private Double totalOrderAmount;

	/**
	 * Customer wallet amount
	 */
	private Double walletContribution;

	private Double customerWalletAmount;

}
