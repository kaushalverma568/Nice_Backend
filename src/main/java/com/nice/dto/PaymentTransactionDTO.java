package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
@Data
public class PaymentTransactionDTO implements Serializable {

	/**
	*
	*/
	private static final long serialVersionUID = 3024131667852109549L;
	private Long orderId;
	private Long customerId;
	private Long deliveryBoyId;
	private Long vendorId;
	private Date paymentDate;
	private String paymentMode;
	private String customerName;
	private String deliveryBoyName;
	private String vendorName;
	private Double amountPaid;
	private String transactionId;
	private String customerEmail;

}
