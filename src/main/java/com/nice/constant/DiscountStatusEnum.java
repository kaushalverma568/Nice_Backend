package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : 27-Mar-2020
 * @description :
 */
@Getter
@AllArgsConstructor
public enum DiscountStatusEnum {
	UPCOMING("Upcoming"), ACTIVE("Active"), EXPIRED("Expired"), CANCELLED("Cancelled");

	String statusValue;

	private static final Map<String, DiscountStatusEnum> DISCOUNT_STATUS = new HashMap<>();
	static {
		for (final DiscountStatusEnum discountStatusEnum : values()) {
			DISCOUNT_STATUS.put(discountStatusEnum.getStatusValue(), discountStatusEnum);
		}
	}

	public static DiscountStatusEnum getByValue(final String value) {
		return DISCOUNT_STATUS.get(value);
	}

	public String getStatusValue() {
		return statusValue;
	}

	public DiscountStatusEnum[] nextStatus() {
		DiscountStatusEnum[] nextType = null;
		switch (this) {
		case UPCOMING:
			nextType = new DiscountStatusEnum[] { ACTIVE, CANCELLED };
			break;
		case ACTIVE:
			nextType = new DiscountStatusEnum[] { EXPIRED, CANCELLED };
			break;
		case EXPIRED:
			break;
		case CANCELLED:
			break;
		default:
			break;
		}
		return nextType;
	}
}
