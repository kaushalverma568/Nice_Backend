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
	public static final String CUSTOMER_ADDRESS = "Customer address";
	public static final String USER_LOGIN = "User Login";
	public static final long OTP_VALIDITY_TIME_IN_MIN = 30;
	public static final String SORT_DIRECTION_ASC = "ASC";
	public static final String SORT_DIRECTION_DESC = "DESC";
	public static final String ADMIN = "Admin";
	public static final String BANNER = "Banner";
	public static final int MAX_BANNER_IMAGES = 3;

	public static final String ACTIVATE_EXPIRE_DISCOUNT = "ACTIVATE_EXPIRE_DISCOUNT";

	public static final String VENDOR_SUBSCRIPTION_EXPIRE = "VENDOR_SUBSCRIPTION_EXPIRE";
	public static final String VENDOR_SUBSCRIPTION_EXPIRE_REMINDER = "VENDOR_SUBSCRIPTION_EXPIRE_REMINDER";

	public static final String EXPIRE_STOCK_SCHEDULER = "EXPIRE_STOCK_SCHEDULER";
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

	public static final String CAN_VIEW = "CAN_VIEW";
	public static final String CAN_EDIT = "CAN_EDIT";
	public static final String CAN_ADD = "CAN_ADD";
	public static final String CAN_DELETE = "CAN_DELETE";

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
	public static final String ORDER_IS_PREPARED = "Order Is Prepared";
	public static final String ORDER_PICKED_UP = "Order Picked Up";
	public static final String PENDING = "Pending";
	public static final String REJECTED = "Rejected";
	public static final String STOCK_ALLOCATED = "Stock Allocated";
	public static final String BUSINESS_CATEGORY_FOOD_ENGLISH = "Food Delivery";
	public static final String BUSINESS_CATEGORY_FOOD_ARABIC = "توصيل طلبات الطعام";
	public static final String VENDOR_STORE_IMAGE = "VENDOR_STORE_IMAGE";
	public static final String VENDOR_STORE_DETAIL_IMAGE = "VENDOR_STORE_DETAIL_IMAGE";
	public static final String VENDOR_FEATURED_IMAGE = "VENDOR_FEATURED_IMAGE";
	public static final String DEFAULT_LANGUAGE = "en";
	public static final String HESABE_CAPTURE = "CAPTURED";
	public static final String HESABE_NOT_CAPTURE = "NOT CAPTURED";

	public static final String BUSINESS_CATEGORY_GROCERY = "Grocery";
	public static final String BUSINESS_CATEGORY_FOOD_DELIVERY = "Food Delivery";

	/**
	 * Delivery Charge
	 */
	public static final String ORDER_DELIVERY_CHARGE = "ORDER_DELIVERY_CHARGE";
	public static final String ORDER_AMOUNT_FOR_FREE_DELIVERY = "ORDER_AMOUNT_FOR_FREE_DELIVERY";
	public static final String DAY_MIN_ORDER_DELIVERED = "DAY_MIN_ORDER_DELIVERED";
	public static final String DELIVERY_CHARGE_DELIVERY_BOY_BELOW_MIN_ORDERS = "DELIVERY_CHARGE_DELIVERY_BOY_BELOW_MIN_ORDERS";
	public static final String DELIVERY_CHARGE_DELIVERY_BOY_ABOVE_MIN_ORDERS = "DELIVERY_CHARGE_DELIVERY_BOY_ABOVE_MIN_ORDERS";

	public static final String CUSTOMER_SIGNUP_REWARD = "CUSTOMER_SIGNUP_REWARD";
	/**
	 * Change this name in changeset from VENDOR_COMISSION TO ADMIN_COMISSION, as it is the admin that takes the comission
	 * and not the vendor, once changed in the settings table , replace the same name here, rest will work fine
	 */
	public static final String ADMIN_COMISSION = "VENDOR_COMISSION";
	/**
	 * Image Dimensions
	 */
	public static final int VENDOR_LIST_IMAGE_WIDTH = 70;
	public static final int VENDOR_LIST_IMAGE_HEIGHT = 70;
	public static final int VENDOR_FEATURE_IMAGE_WIDTH = 270;
	public static final int VENDOR_FEATURE_IMAGE_HEIGHT = 110;
	public static final int VENDOR_DETAIL_IMAGE_WIDTH = 375;
	public static final int VENDOR_DETAIL_IMAGE_HEIGHT = 164;
	public static final int PRODUCT_LIST_IMAGE_WIDTH = 70;
	public static final int PRODUCT_LIST_IMAGE_HEIGHT = 70;
	public static final int PRODUCT_DETAIL_IMAGE_WIDTH = 375;
	public static final int PRODUCT_DETAIL_IMAGE_HEIGHT = 164;
	public static final int BUSINESS_CATEGORY_IMAGE_WIDTH = 154;
	public static final int BUSINESS_CATEGORY_IMAGE_HEIGHT = 154;
	public static final int CATEGORY_IMAGE_WIDTH = 70;
	public static final int CATEGORY_IMAGE_HEIGHT = 70;
	public static final int CUISINE_IMAGE_WIDTH = 70;
	public static final int CUISINE_IMAGE_HEIGHT = 70;
	public static final int SUB_CATEGORY_IMAGE_WIDTH = 70;
	public static final int SUB_CATEGORY_IMAGE_HEIGHT = 70;
	public static final int SLIDER_IMAGE_WIDTH = 335;
	public static final int SLIDER_IMAGE_HEIGHT = 170;

}
