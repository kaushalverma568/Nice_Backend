package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "SendingSMSResult", namespace = "http://itbsms.com/")
@XmlAccessorType(XmlAccessType.FIELD)
public class SendingSMSResult implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	private String Message;
	private boolean Result;
	private BigDecimal NetPoints;
	private String messageId;
	private List<String> RejectedNumbers;

	@XmlElement
	public String getMessage() {
		return Message;
	}

	public void setMessage(final String message) {
		Message = message;
	}

	@XmlElement
	public boolean isResult() {
		return Result;
	}

	public void setResult(final boolean result) {
		Result = result;
	}

	@XmlElement
	public BigDecimal getNetPoints() {
		return NetPoints;
	}

	public void setNetPoints(final BigDecimal netPoints) {
		NetPoints = netPoints;
	}

	public String getMessageId() {
		return messageId;
	}

	public void setMessageId(final String messageId) {
		this.messageId = messageId;
	}

	@XmlElement
	public List<String> getRejectedNumbers() {
		return RejectedNumbers;
	}

	public void setRejectedNumbers(final List<String> rejectedNumbers) {
		RejectedNumbers = rejectedNumbers;
	}

}
