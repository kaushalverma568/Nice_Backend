package com.nice;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Data;

@Data
public class SendingSMSResult implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@JsonProperty("Message")
	private String message;
	@JsonProperty("Result")
	private Boolean result;
	@JsonProperty("NetPoints")
	private BigDecimal netPoints;
	@JsonProperty("messageId")
	private String messageId;
	@JsonProperty("RejectedNumbers")
	private List<String> rejectedNumbers;
}
