package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
public final class NotificationMessageConstantsArabic {

	private NotificationMessageConstantsArabic() {
		super();
	}

	public static final String NEW_PROFILE_VALIDATE = "You have receive new profile to validate ";
	public static final String NEW_ORDER_DELIVERY = "New Order for Delivery";
	public static final String NEW_TICKET_VALIDATE = "You receive the new ticket to validate for the ";
	public static final String NEW_ORDER_VENDOR = "You have received one new cart order with order id ";
	public static final String ORDER_DELIVERY_VENDOR = " has delivered an order id ";

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
}
