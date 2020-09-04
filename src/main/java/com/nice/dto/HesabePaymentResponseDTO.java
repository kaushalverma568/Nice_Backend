package com.nice.dto;

import java.io.Serializable;
import java.util.Map;

import lombok.Data;

@Data
public class HesabePaymentResponseDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = -945053115594353073L;
	String message;
	String code;
	String status;
	Map<String, String> response;
}
