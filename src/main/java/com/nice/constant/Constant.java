/**
 *
 */
package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jun-2020
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

	public static final String ACTIVATE_EXPIRE_DISCOUNT = "ACTIVATE_EXPIRE_DISCOUNT";

	public static final String VENDOR_SUBSCRIPTION_EXPIRE = "VENDOR_SUBSCRIPTION_EXPIRE";
	public static final String VENDOR_SUBSCRIPTION_EXPIRE_REMINDER = "VENDOR_SUBSCRIPTION_EXPIRE_REMINDER";

	public static final String EXPIRE_STOCK_SCHEDULER = "EXPIRE_STOCK_SCHEDULER";
	/**
	 * average speed of delivery boy is 40 km/h
	 */
	public static final Double DELIVERY_BOY_AVERAGE_SPEED = 40d;
	/**
	 * maximum distance of vendor from customer to fetch in customer app (in km)
	 */
	public static final Double MAX_DISTANCE_FROM_CUSTOMER = 20d;
	/**
	 * maximum assignment try count for sending notification to delivery boy
	 */
	public static final Integer MAX_ASSIGNMENT_TRY_COUNT = 3;
	/**
	 * a time after we have to send notification for order
	 */
	public static final Long NOTIFICATION_SENDING_TIME_IN_MILIS = 45000L;

	/**
	 * Settings Flags:
	 */

	private static String sendSMS = "false";
	private static String sendEmail = "false";
	private static String smsApiKey = "default";

	/**
	 * Sets the values of static fields</b> The value specified is set for the key specified in the arguments</br>
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
	 * @param  key
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
	public static final String ANONYMOUS_USER = "anonymousUser";

	public static final String CLIENT_ID = "kody-client";
	public static final String SECRET_ID = "kody-secret";
	public static final String UNAUTHORIZED = "unauthorized";
	public static final String AWAITING_APPROVAL = "Awaiting Approval";
	public static final String AVAILABLE = "Available";
	public static final String IN_PROCESS = "In-Process";
	public static final String RESERVED = "Reserved";
	public static final String DELIVERED = "Delivered";
	public static final String REPLACE_REQUESTED = "Replace Requested";
	public static final String REPLACE_PROCESSED = "Replace Processed";
	public static final String REPLACED = "Replaced";
	public static final String CANCELLED = "Cancelled";
	public static final String RETURNED = "Returned";
	public static final String CONFIRMED = "Confirmed";
	public static final String RETURN_REQUESTED = "Return Requested";
	public static final String RETURN_PROCESSED = "Return Processed";
	public static final String ORDER_IS_READY = "Order Is Ready";
	public static final String ORDER_PICKED_UP = "Order Picked Up";
	public static final String PENDING = "Pending";
	public static final String BUSINESS_CATEGORY_FOOD = "Food Delivery";
}
