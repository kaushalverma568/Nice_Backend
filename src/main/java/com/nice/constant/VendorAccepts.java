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
 * @date   : 29-06-2020
 */
@Getter
@AllArgsConstructor
public enum VendorAccepts {
	/**
	 * Vendor supports either replacement of order or return of order
	 */
	REPLACE("Replace"), RETURN("Return");

	String statusValue;

	private static final Map<String, VendorAccepts> VENDOR_ACCEPTS_LIST = new HashMap<>();
	static {
		for (final VendorAccepts vendorAccepts : values()) {
			VENDOR_ACCEPTS_LIST.put(vendorAccepts.getStatusValue(), vendorAccepts);
		}
	}

	public static VendorAccepts getByValue(final String value) {
		return VENDOR_ACCEPTS_LIST.get(value);
	}
}
