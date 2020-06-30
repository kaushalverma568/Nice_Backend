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
public enum DeliveryType {
	/**
	 * Vendor supports delivery type from one of this option
	 */
	PICKUP("Pick-Up"), DELIVERY("Delivery"), BOTH("Both");

	String statusValue;

	private static final Map<String, DeliveryType> DELIVERY_TYPE_LIST = new HashMap<>();
	static {
		for (final DeliveryType deliveryType : values()) {
			DELIVERY_TYPE_LIST.put(deliveryType.getStatusValue(), deliveryType);
		}
	}

	public static DeliveryType getByValue(final String value) {
		return DELIVERY_TYPE_LIST.get(value);
	}
}
