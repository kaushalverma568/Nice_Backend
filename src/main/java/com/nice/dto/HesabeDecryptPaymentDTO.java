package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

@Data
public class HesabeDecryptPaymentDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = -945053115594353073L;
	String message;
	String code;
	String status;
	HesabePaymentDTO response;

}
