package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TicketStatusEnum {

	PENDING("Pending"), ACKNOWLEDGED("Acknowledged"), RESOLVED("Resolved");

	String statusValue;

	private static final Map<String, TicketStatusEnum> ENQUIRY_STATUS = new HashMap<>();
	static {
		for (final TicketStatusEnum ticketStatusEnum : values()) {
			ENQUIRY_STATUS.put(ticketStatusEnum.getStatusValue(), ticketStatusEnum);
		}
	}

	public static TicketStatusEnum getByValue(final String value) {
		return ENQUIRY_STATUS.get(value);
	}

	public TicketStatusEnum[] nextStatus() {
		TicketStatusEnum[] nextType = null;
		switch (this) {

		case PENDING:
			nextType = new TicketStatusEnum[] { ACKNOWLEDGED, RESOLVED };
			break;
		case ACKNOWLEDGED:
			nextType = new TicketStatusEnum[] { RESOLVED };
			break;
		case RESOLVED:
			break;
		default:
			break;
		}
		return nextType;
	}

}
