package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Mar-2020
 * @description :
 */
@Getter
@AllArgsConstructor
public enum TicketReasonType {

	DELIVERY_BOY("DELIVERY_BOY"), VENDOR("VENDOR"), CUSTOMER("CUSTOMER"), CANCEL("CANCEL"), REPLACE("REPLACE"), REJECT("REJECT"), RETURN("RETURN");

	String statusValue;

	private static final Map<String, TicketReasonType> TICKET_REASON_TYPE_LIST = new HashMap<>();
	static {
		for (final TicketReasonType TicketReasonType : values()) {
			TICKET_REASON_TYPE_LIST.put(TicketReasonType.getStatusValue(), TicketReasonType);
		}
	}

	public static TicketReasonType getByValue(final String value) {
		return TICKET_REASON_TYPE_LIST.get(value);
	}
}
