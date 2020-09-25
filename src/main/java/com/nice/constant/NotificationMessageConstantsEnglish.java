package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
public final class NotificationMessageConstantsEnglish {

	private NotificationMessageConstantsEnglish() {
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
		message.append("Congratulations! Your order ").append(orderId).append(" worth ").append(orderAmount).append(" has been placed successfully.");
		return message.toString();
}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getCancelOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have cancelled an order ").append(orderId).append(". If it is cancelled by mistake please place the order again.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String getOrderStatusUpdateMessageExceptDelivery(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("We have process your order no. ").append(orderId).append(". We will delivery it shortly.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" has been delivered to you successfully.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String orderItemReplaceSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order item/s for order no. ").append(orderId).append(" has been replaced successfully.");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String orderItemReturnSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order item/s for order no. ").append(orderId).append(" has been returned successfully.");
		return message.toString();
	}
}
