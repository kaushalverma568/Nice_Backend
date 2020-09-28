package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
public final class NotificationMessageConstantsArabic {

	private NotificationMessageConstantsArabic() {
		super();
	}

	/**
	 *
	 * @param name
	 * @return
	 */
	public static String getNewProfileMessage(final String name) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have receive new profile to validate ").append(name);
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getNewOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("New Order for Delivery ").append(orderId.toString());
		return message.toString();
	}

	/**
	 *
	 * @param name
	 * @return
	 */
	public static String getNewTicketMessage(final String name) {
		StringBuilder message = new StringBuilder();
		message = message.append("You receive the new ticket to validate for the ").append(name);
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getNewOrderToVendorMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have received one new cart order with order id ").append(orderId.toString());
		return message.toString();
	}

	/**
	 *
	 * @param name
	 * @param orderId
	 * @return
	 */
	public static String getDeliveryOrderToVendorMessage(final String name, final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append(name).append(" has delivered an order id ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param orderId
	 * @param orderAmount
	 * @return
	 */
	public static String getCreateOrderMessage(final Long orderId, final Double orderAmount) {
		StringBuilder message = new StringBuilder();
		message.append("تهانينا! طلبك ").append(orderId).append(" يستحق ").append(orderAmount).append(" تم وضعه بنجاح.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getCancelOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("لقد ألغيت الطلب ").append(orderId).append(". إذا تم إلغاؤه عن طريق الخطأ ، يرجى تقديم الطلب مرة أخرى.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getOrderStatusUpdateMessageExceptDelivery(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("لقد قمنا بمعالجة رقم طلبك ").append(orderId).append(". سنقوم بتسليمه قريبا.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("رقم طلبك ").append(orderId).append(" تم تسليمها لك بنجاح.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String orderItemReplaceSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("عناصر الطلب الخاص بك لرقم الطلب ").append(orderId).append(" تم استبداله بنجاح.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String orderItemReturnSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("عناصر الطلب الخاص بك لرقم الطلب ").append(orderId).append(" تم إرجاعه بنجاح.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getRejectedOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("آسف! طلبك ").append(orderId).append("  من قبل البائع. إذا كنت قد دفعت مقابل الطلب عبر الإنترنت ، فسيتم رد المبلغ إلى محفظتك الرائعة");
		return message.toString();
	}
}
