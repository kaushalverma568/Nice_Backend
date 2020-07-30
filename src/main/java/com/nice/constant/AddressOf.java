/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : Jul 30, 2020
 */
@Getter
@AllArgsConstructor
public enum AddressOf {

	HOME("Home"), WORK("Work");

	private String statusValue;

	private static final Map<String, AddressOf> PAYMENT_METHOD_LIST = new HashMap<>();
	static {
		for (final AddressOf addressOf : values()) {
			PAYMENT_METHOD_LIST.put(addressOf.getStatusValue(), addressOf);
		}
	}

	public static AddressOf getByValue(final String value) {
		return PAYMENT_METHOD_LIST.get(value);
	}
}
