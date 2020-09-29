package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public final class NotificationMessageConstantsArabic {

	private NotificationMessageConstantsArabic() {
		super();
	}

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

	public static String getPayoutMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Your weekly/monthy Payment is Release by admin, Please check you added Bank Account for more update");
		return message.toString();

	/**
	 *
	 * @param orderId
	 * @param amount
	 * @return
	 */
	public static String getRefundOrderMessage(final Long orderId, final Double amount) {
		StringBuilder message = new StringBuilder();
		message.append("استرداد الخاص بك ل ").append(amount).append(" ضد الأمر لا.").append(orderId).append(" تمت إضافته إلى محفظتك");
		return message.toString();
	}

	/**
	 *
	 * @return
	 */
	public static String profileSuspendedForCustomer() {
		StringBuilder message = new StringBuilder();
		message.append("حساب العميل الخاص بك على ").append("لطيف").append(" تم تعليقه مؤقتًا لمخالفته شروط وأحكام ").append("لطيف");
		return message.toString();
	}

	/**
	 * @param orderId
	 * @param deliveryBoyName
	 * @return
	 */
	public static String getOrderAcceptedMessageToCustomer(final Long orderId, final String deliveryBoyName) {
		StringBuilder message = new StringBuilder();
		message.append("طلبك ").append(orderId).append("تم قبوله من قبل فتى التوصيل").append(deliveryBoyName);
		return message.toString();
	}

	/**
	 * @param orderId
	 * @param orderStatus
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId, final String orderStatus) {
		StringBuilder message = new StringBuilder();
		message.append("طلبك ").append(orderId).append(" كان ").append(orderStatus).append("بنجاح");
		return message.toString();
	}
}
