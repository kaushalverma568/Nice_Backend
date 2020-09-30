package com.nice.constant;

import java.time.ZoneId;
import java.util.Date;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public final class NotificationMessageConstantsArabic {

	private NotificationMessageConstantsArabic() {
		super();
	}

	public static final String DELIVERY_BOY_PAYOUT_SUBJECT = "Delivery Boy Payment";
	public static final String VENDOR_PAYOUT_SUBJECT = "Vendor Payment";
	public static final String ACCOUNT_ACCTIVATION_SUBJECT = "Account Activation";

	/**
	 * @param  name
	 * @return
	 */
	public static String getNewProfileMessage(final String name) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have receive new profile to validate ").append(name);
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getNewOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("New Order for Delivery ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  name
	 * @return
	 */
	public static String getNewTicketMessage(final String name) {
		StringBuilder message = new StringBuilder();
		message = message.append("You receive the new ticket to validate for the ").append(name);
		return message.toString();
	}

	public static String getPayoutMessage(final String userName, final Date paidOn) {
		StringBuilder message = new StringBuilder();
		message = message.append(" Dear ").append(userName).append(", Your Payment date ")
				.append(paidOn.toInstant().atZone(ZoneId.systemDefault()).toLocalDate().toString())
				.append(" is Release by admin, Please check you added Bank Account for more Update");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getNewOrderToVendorMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have received one new cart order with order id ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  name
	 * @param  orderId
	 * @return
	 */
	public static String getDeliveryOrderToVendorMessage(final String name, final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append(name).append(" has delivered an order id ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  orderAmount
	 * @return
	 */
	public static String getCreateOrderMessage(final Long orderId, final Double orderAmount) {
		StringBuilder message = new StringBuilder();
		message.append("تهانينا! طلبك ").append(orderId).append(" يستحق ").append(orderAmount).append(" تم وضعه بنجاح.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getCancelOrderMessage(final Long orderId, final boolean cancelledByCustomer) {
		StringBuilder message = new StringBuilder();
		if (cancelledByCustomer) {
			message.append("لقد ألغيت الطلب ").append(orderId).append(". إذا تم إلغاؤه عن طريق الخطأ ، يرجى تقديم الطلب مرة أخرى.");
		} else {
			message.append("طلبك").append(orderId).append("تم إلغاؤه من قبل المشرف ، آسف لخيبة الأمل.");
		}

		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getOrderStatusUpdateMessageExceptDelivery(final Long orderId, final String currentStatus) {
		StringBuilder message = new StringBuilder();
		message.append("طلبك لا.").append(orderId).append("يكون").append(currentStatus).append(". سنقوم بتسليمه قريبا.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("رقم طلبك ").append(orderId).append(" تم تسليمها لك بنجاح.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String orderItemReplaceSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("عناصر الطلب الخاص بك لرقم الطلب ").append(orderId).append(" تم استبداله بنجاح.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String orderItemReturnSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("عناصر الطلب الخاص بك لرقم الطلب ").append(orderId).append(" تم إرجاعه بنجاح.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getRejectedOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("آسف! طلبك ").append(orderId).append("  من قبل البائع. إذا كنت قد دفعت مقابل الطلب عبر الإنترنت ، فسيتم رد المبلغ إلى محفظتك الرائعة");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String replaceRequestInitiated(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have received replacement request for the order id : ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String returnRequestInitiated(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have received return request for the order id : ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String cancelOrderByAdmin(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("order ").append(orderId.toString()).append(" has cancel by admin,Sorry for disappointment");
		return message.toString();
	}

	public static String getPayoutMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Your weekly/monthy Payment is Release by admin, Please check you added Bank Account for more update");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String getRefundOrderMessage(final Long orderId, final Double amount) {
		StringBuilder message = new StringBuilder();
		message.append("استرداد الخاص بك ل ").append(amount).append(" ضد الأمر لا.").append(orderId).append(" تمت إضافته إلى محفظتك");
		return message.toString();
	}

	/**
	 * @return
	 */
	public static String profileSuspendedForCustomer() {
		StringBuilder message = new StringBuilder();
		message.append("حساب العميل الخاص بك على ").append("لطيف").append(" تم تعليقه مؤقتًا لمخالفته شروط وأحكام ").append("لطيف");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  deliveryBoyName
	 * @return
	 */
	public static String getOrderAcceptedMessageToCustomer(final Long orderId, final String deliveryBoyName) {
		StringBuilder message = new StringBuilder();
		message.append("طلبك ").append(orderId).append("تم قبوله من قبل فتى التوصيل").append(deliveryBoyName);
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  orderStatus
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId, final String orderStatus) {
		StringBuilder message = new StringBuilder();
		message.append("طلبك ").append(orderId).append(" كان ").append(orderStatus).append("بنجاح");
		return message.toString();
	}

	/**
	 * @param  ticketId
	 * @return
	 */
	public static String resolveTicketByAdmin(final Long ticketId) {
		StringBuilder message = new StringBuilder();
		message.append("Your raised Ticket ").append(ticketId.toString()).append(" is resolved by admin, You can check the more details  in ticket section");
		return message.toString();
	}

	/**
	 * Email related notification msg starts from here
	 */
	public static String approveVendorProfileSubject() {
		StringBuilder message = new StringBuilder();
		message.append("Approve the profile");
		return message.toString();
	}

	public static String rejectVendorProfileSubject() {
		StringBuilder message = new StringBuilder();
		message.append("Reject the profile");
		return message.toString();
	}

	public static String suspendVendorProfileSubject() {
		StringBuilder message = new StringBuilder();
		message.append("Suspend profile");
		return message.toString();
	}

	public static String resumeVendorProfileSubject() {
		StringBuilder message = new StringBuilder();
		message.append("Resume the profile");
		return message.toString();
	}

	public static String expiredVendorSubscriptionSubject() {
		StringBuilder message = new StringBuilder();
		message.append("Subscription Expired");
		return message.toString();
	}

	public static String expiredReminderVendorSubscriptionSubject() {
		StringBuilder message = new StringBuilder();
		message.append("Reminder for Subscription Expire");
		return message.toString();
	}

	public static String approveVendorProfileMessage() {
		StringBuilder message = new StringBuilder();
		message.append(
				"Nice has Review the Your profile, & your profile is approved by our team Now to be part of the Nice, and You are able to  login into the system.");
		return message.toString();
	}

	public static String rejectVendorProfileMessage() {
		StringBuilder message = new StringBuilder();
		message.append("Nice has Review the Your profile, & your profile is not approved by our team Sorry for inconvenience.\r\n"
				+ "For further information you can contact to our customer care");
		return message.toString();
	}

	public static String suspendVendorProfileMessage() {
		StringBuilder message = new StringBuilder();
		message.append(
				"This is to notify you that your vendor account on Nice Application has been temporarily  suspended for violating the terms and conditions of Nice Application."
						+ "Please the contact to customer care for further information and You will be notified once your account will be resume");
		return message.toString();
	}

	public static String resumeVendorProfileMessage() {
		StringBuilder message = new StringBuilder();
		message.append(
				"This is to notify you that your vendor account on Nice Application has  been Resume our side. Now you are able to use the our panel service.\r\n"
						+ "Please the contact to customer care for further information ");
		return message.toString();
	}

	public static String vendorSubscriptionExpiredMessage(final Double amount) {
		StringBuilder message = new StringBuilder();
		message.append("You Nice Application subscription plan of KD").append(amount.toString()).append(
				" has expired on <date>. To Continue enjoying  Nice Application services, renew the subscription plan, or purchase a new subscription.\r\n"
						+ "\r\n" + "To renew or make a purchase of subscription plan click ");
		return message.toString();
	}

	public static String vendorSubscriptionExpiredReminderMessage(final Double amount, final Date expiryDate) {
		StringBuilder message = new StringBuilder();
		message.append("Your current subscription plan of KD").append(amount.toString()).append("for Nice Application is going to expire on ")
				.append(expiryDate.toString()).append(". After the current plan expiry, your services will stop working if you do not renew your plan.");
		return message.toString();
	}

	public static String deliveryBoyActivation() {
		StringBuilder message = new StringBuilder();
		message.append("Your Nice account activated by admin ,Now you can Login-in into the Nice delivery boy application Thank you.");
		return message.toString();
	}
}
