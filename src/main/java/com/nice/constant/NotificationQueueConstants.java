package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public final class NotificationQueueConstants {

	private NotificationQueueConstants() {
		super();
	}

	public static final String GENERAL_QUEUE = "Nice General Queue";
	public static final String CUSTOMER_REGISTRATION = "Customer Registration";
	public static final String EMAIL_VERIFICATION = "Email Verification";
	public static final String FORGOT_PASS = "Forgot Password";
	public static final String NON_NOTIFICATION_QUEUE = "Nice Non Notification Queue";
	public static final String SEND_OTP = "Send OTP";
	public static final String SMS_QUEUE = "Nice SMS Queue";
	public static final String VENDOR_SUBSCRIPTION_EXPIRY_REMINDER = "Vendor Subscription Expiry Reminder Queue";
	public static final String ACCEPT_ORDER_PUSH_NOTIFICATION_QUEUE = "Accept Order Push Notification Queue";
	public static final String ACCEPT_ORDER_PUSH_NOTIFICATION = "Accept Order Push Notification";
	public static final String PUSH_NOTIFICATION_QUEUE = "Push Notification Queue";
	public static final String PLACE_ORDER = "Place Order Queue";
	public static final String VENDOR_STATUS_CHANGE = "Vendor status change";
	public static final String NEW_VENDOR_PUSH_NOTIFICATION = "New Vendor Push Notification";
	public static final String NEW_DB_PUSH_NOTIFICATION = "New Delivery Boy Push Notification";
	public static final String NEW_TICKET_PUSH_NOTIFICATION = "New Ticket Push Notification";
	public static final String RESOLVE_TICKET_PUSH_NOTIFICATION = "Resolve Ticket Push Notification";
	public static final String NEW_ORDER_PUSH_NOTIFICATION = "New Order Push Notification";
	public static final String ORDER_DELIVERY_PUSH_NOTIFICATION = "Order Delivery Push Notification";
	public static final String GENERAL_PUSH_NOTIFICATION_QUEUE = "Nice General Push Notification Queue";
	public static final String DELIVERY_BOY_ACCOUNT_ACTIVATION = "Delivery Boy Account Activation";
	public static final String ORDER_PREPARED = "Order Is Prepared Notification To Delivery Boy";
	public static final String CANCEL_ORDER_PUSH_NOTIFICATION_DELIVERY_BOY = "Cancel Order Push Notification Delivery Boy";

	/**
	 * push Notification to vendor
	 */
	public static final String REPLACE_ORDER_PUSH_NOTIFICATION_VENDOR = "Replace Order Push Notification Vendor";
	public static final String RETURN_ORDER_PUSH_NOTIFICATION_VENDOR = "Return Order Push Notification Vendor";
	public static final String CANCEL_ORDER_PUSH_NOTIFICATION_VENDOR = "Cancel Order Push Notification Vendor";
	/**
	 * Push notifications for orders to customer
	 */
	public static final String PLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER = "Place Order Push Notification Customer";
	public static final String DELIVER_ORDER_PUSH_NOTIFICATION_CUSTOMER = "Deliver Order Push Notification Customer";
	public static final String ORDER_STATUS_CHANGE_PUSH_NOTIFICATION_CUSTOMER = "Order Status Change Push Notification Customer";
	public static final String REPLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER = "Replace Order Push Notification Customer";
	public static final String RETURN_ORDER_PUSH_NOTIFICATION_CUSTOMER = "Return Order Push Notification Customer";
	public static final String CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER = "Cancel Order Push Notification Customer";
	public static final String CANCEL_ORDER_BY_ADMIN_PUSH_NOTIFICATION_CUSTOMER = "Cancel Order By Admin Push Notification Customer";
	public static final String REJECT_ORDER_PUSH_NOTIFICATION_CUSTOMER = "REJECT_ORDER_PUSH_NOTIFICATION_CUSTOMER";
	public static final String PAYOUT = "Payout";
	public static final String REFUND_ORDER_PUSH_NOTIFICATION_CUSTOMER = "REFUND_ORDER_PUSH_NOTIFICATION_CUSTOMER";
	public static final String ACCEPT_ORDER_PUSH_NOTIFICATION_CUSTOMER = "ACCEPT_ORDER_PUSH_NOTIFICATION_CUSTOMER";
	/**
	 * Email notifications for orders to customer
	 */
	public static final String PLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER = "Place Order Email Notification Customer";
	public static final String REPLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER = "Replace Order Email Notification Customer";
	public static final String RETURN_ORDER_EMAIL_NOTIFICATION_CUSTOMER = "Return Order Email Notification Customer";
	public static final String CANCEL_ORDER_EMAIL_NOTIFICATION_CUSTOMER = "Cancel Order Email Notification Customer";
	public static final String DELIVER_ORDER_EMAIL_NOTIFICATION_CUSTOMER = "Deliver Order Email Notification Customer";

	/**
	 * Deactive notification to customer for deactivation.
	 */
	public static final String DEACTIVE_CUSTOMER_NOTIFICATION = "Deactive customer push notification";

}
