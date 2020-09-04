package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

@Data
public class HesabePaymentDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -8280698366297439780L;

	String resultCode;
	Double amount;
	String paymentToken;
	String paymentId;
//	Timestamp paidOn;
	String orderReferenceNumber;
	String variable1;
	String variable2;
	String variable3;
	String variable4;
	String variable5;
	Integer method;
	Double administrativeCharge;
}
