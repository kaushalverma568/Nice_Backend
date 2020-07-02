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
	public static final String ADMIN = "Admin";
	public static final String DELIVERY_BOY = "Delivery Boy";
	public static final String USER = "USER";
	public static final String BANNER = "Banner";
	public static final int MAX_BANNER_IMAGES = 3;

	/**
	 * Settings Flags:
	 */

	private static String sendSMS = "false";
	private static String sendEmail = "false";
	private static String smsApiKey = "default";

	/**
	 * Sets the values of static fields</b> The value specified is set for the key
	 * specified in the arguments</br>
	 *
	 * @param key
	 * @param value
	 */
	public static void setSettingsValue(final String key, final String value) {
		if ("SEND_SMS".equalsIgnoreCase(key)) {
			sendSMS = value;
		} else if ("SEND_EMAIL".equalsIgnoreCase(key)) {
			sendEmail = value;
		} else if ("SMS_API_KEY".equalsIgnoreCase(key)) {
			smsApiKey = value;
		}
	}

	/**
	 * @param key
	 * @return
	 */
	public static String getSettingsValue(final String key) {
		if ("SEND_SMS".equalsIgnoreCase(key)) {
			return sendSMS;
		} else if ("SEND_EMAIL".equalsIgnoreCase(key)) {
			return sendEmail;
		} else if ("SMS_API_KEY".equalsIgnoreCase(key)) {
			return smsApiKey;
		}
		return null;
	}

	public static final String CAN_VIEW = "CAN_VIEW";
	public static final String CAN_VIEW_LIST = "CAN_VIEW_LIST";
	public static final String CAN_EDIT = "CAN_EDIT";
	public static final String CAN_ADD = "CAN_ADD";
	public static final String CAN_DELETE = "CAN_DELETE";
	public static final String CAN_IMPORT = "CAN_IMPORT";
	public static final String CAN_EXPORT = "CAN_EXPORT";

	public static final String CLIENT_ID = "kody-client";
	public static final String SECRET_ID = "kody-secret";

}
