/**
 *
 */
package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Jun-2020
 */
public final class Constant {

	private Constant() {
		super();
	}

	/**
	 * DB Encryption Salt
	 */
	public static final String KEY_ENCRYPTION_SALT = "kody_encryption_key";
	public static final String PAYMENT_GATEWAY_USER_NAME = "PAYMENT_GATEWAY_USER_NAME";
	public static final String PAYMENT_GATEWAY_SECRET = "PAYMENT_GATEWAY_SECRET";

	public static final String GRANT_TYPE = "password";
	public static final String COUNTRY = "Country";
	public static final String STATE = "State";
	public static final String CITY = "City";
	public static final String PINCODE = "Pincode";
	public static final String CATEGORY = "Category";
	public static final String SUB_CATEGORY = "Sub Category";
	public static final String CUSTOMER = "Customer";
	public static final String CUSTOMER_ADDRESS = "Customer address";
	public static final String USER_LOGIN = "User Login";
	public static final long OTP_VALIDITY_TIME_IN_MIN = 30;
	public static final String SORT_TYPE_ASC = "ASC";
	public static final String SORT_TYPE_DESC = "DESC";

	public static final String CAN_VIEW = "CAN_VIEW";
	public static final String CAN_VIEW_LIST = "CAN_VIEW_LIST";
	public static final String CAN_EDIT = "CAN_EDIT";
	public static final String CAN_ADD = "CAN_ADD";
	public static final String CAN_DELETE = "CAN_DELETE";
	public static final String CAN_IMPORT = "CAN_IMPORT";
	public static final String CAN_EXPORT = "CAN_EXPORT";

}
