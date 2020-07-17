package com.nice.dto;

import java.io.Serializable;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 15-07-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
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