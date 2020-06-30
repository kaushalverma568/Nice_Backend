/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
 */
@Getter
@AllArgsConstructor
public enum PaymentMethod {
	/**
	 * Vendor supports payment method from one of this option
	 */
	ONLINE("Online"), OFFLINE("Offline"), BOTH("Both");

	String statusValue;

	private static final Map<String, PaymentMethod> PAYMENT_METHOD_LIST = new HashMap<>();
	static {
		for (final PaymentMethod paymentMethod : values()) {
			PAYMENT_METHOD_LIST.put(paymentMethod.getStatusValue(), paymentMethod);
		}
	}

	public static PaymentMethod getByValue(final String value) {
		return PAYMENT_METHOD_LIST.get(value);
	}
}
