/**
 *
 */
package com.nice.constant;

import java.util.ArrayList;
import java.util.List;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
public enum UserType {

	CUSTOMER, USER, DELIVERY_BOY;

	/**
	 * add admin panel's users in list (do not add super admin)
	 */
	public static final List<String> ADMIN_PANEL_USER_LIST = new ArrayList<>();
	static {
		ADMIN_PANEL_USER_LIST.add(USER.name());
	}
}
