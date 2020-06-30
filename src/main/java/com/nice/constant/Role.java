package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Jun-2020
 */
@Getter
@AllArgsConstructor
public enum Role {

	CUSTOMER("CUSTOMER"), SUPER_ADMIN("SUPER_ADMIN"), ADMIN("ADMIN"), MANAGER("MANAGER"), DELIVERY_BOY("DELIVERY_BOY"), VENDOR("VENDOR");

	String statusValue;

	private static final Map<String, Role> ROLE_LIST = new HashMap<>();
	static {
		for (final Role role : values()) {
			ROLE_LIST.put(role.getStatusValue(), role);
		}
	}

	public static Role getByValue(final String value) {
		return ROLE_LIST.get(value);
	}

}
