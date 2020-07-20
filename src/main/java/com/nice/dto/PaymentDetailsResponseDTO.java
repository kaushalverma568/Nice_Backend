package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class PaymentDetailsResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 5963020762997885569L;

	private Long id;

	private Integer noOfOrders;

	private Long deliveryBoyId;

	private String deliveryBoyName;

	private Double paymentAmount;

	private String transactionNo;
}