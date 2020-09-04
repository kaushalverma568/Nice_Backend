package com.nice.dto;

import java.util.Map;

import lombok.Data;

@Data
public class EncryptResponseDTO {

	String status;
	String code;
	String message;
	Map<String, String> response;
}
