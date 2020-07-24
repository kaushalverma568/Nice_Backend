/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Getter
@AllArgsConstructor
public enum CartItemStatus {
	PENDING("PENDING"), PAYMENT_WAITING("PAYMENT_WAITING"), PAYMENT_FAIL("PAYMENT_FAIL"), PAYMENT_SUCCESS("PAYMENT_SUCCESS");

	String statusValue;

	private static final Map<String, CartItemStatus> CART_ITEM_STATUS = new HashMap<>();
	static {
		for (final CartItemStatus cartItemStatus : values()) {
			CART_ITEM_STATUS.put(cartItemStatus.getStatusValue(), cartItemStatus);
		}
	}

	public static CartItemStatus getByValue(final String value) {
		return CART_ITEM_STATUS.get(value);
	}
}
