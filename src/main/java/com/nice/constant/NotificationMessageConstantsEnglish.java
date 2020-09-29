package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
public final class NotificationMessageConstantsEnglish {

	private NotificationMessageConstantsEnglish() {
		super();
	}

	/**
	 * @param name
	 * @return
	 */
	public static String getNewProfileMessage(final String name) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have receive new profile to validate ").append(name);
		return message.toString();
	}

	/**
	 * @param orderId
	 * @return
	 */
	public static String getNewOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("New Order for Delivery ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param name
	 * @return
	 */
	public static String getNewTicketMessage(final String name) {
		StringBuilder message = new StringBuilder();
		message = message.append("You receive the new ticket to validate for the ").append(name);
		return message.toString();
	}

	/**
	 * @param orderId
	 * @return
	 */
	public static String getNewOrderToVendorMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have received one new cart order with order id ").append(orderId.toString());
		return message.toString();
	}

	/**
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
	 * @param name
	 * @param orderId
	 * @return
	 */
	public static String getPayoutMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Your weekly/monthy Payment is Release by admin, Please check you added Bank Account for more update");
		return message.toString();
	}

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
	 * @param orderId
	 * @return
	 */
	public static String getCancelOrderMessage(final Long orderId, final boolean cancelByCustomer) {
		StringBuilder message = new StringBuilder();
		if (cancelByCustomer) {
			message.append("You have cancelled an order ").append(orderId).append(". If it is cancelled by mistake please place the order again.");
		} else {
			message.append("Your order ").append(orderId).append("is cancelled by admin, sorry for disappointment.");
		}

		return message.toString();
	}

	/**
	 * @param orderId
	 * @return
	 */
	public static String getOrderStatusUpdateMessageExceptDelivery(final Long orderId, final String currentStatus) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" is ").append(currentStatus).append(". We will delivery it shortly.");
		return message.toString();
	}

	/**
	 * @param orderId
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId, final String orderStatus) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" has been sucessfully ").append(orderStatus);
		return message.toString();
	}

	/**
	 * @param orderId
	 * @return
	 */
	public static String orderItemReplaceSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order item/s for order no. ").append(orderId).append(" has been replaced successfully.");
		return message.toString();
	}

	/**
	 * @param orderId
	 * @return
	 */
	public static String orderItemReturnSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order item/s for order no. ").append(orderId).append(" has been returned successfully.");
		return message.toString();
	}

	/**
	 * @param orderId
	 * @param orderAmount
	 * @return
	 */
	public static String getRejectedOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Sorry! Your order ").append(orderId)
				.append(" has been rejected by vendor. If you have paid for the order online the amount would be refunded your nice wallet");
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String replaceRequestInitiated(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have received replacement request for the order id : ").append(orderId.toString());
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String returnRequestInitiated(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have received return request for the order id : ").append(orderId.toString());
		return message.toString();
	}

	/**
	 *
	 * @param orderId
	 * @return
	 */
	public static String cancelOrderByAdmin(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("order ").append(orderId.toString()).append(" has cancel by admin,Sorry for disappointment");
		return message.toString();
	}

	/**
	 *
	 * @param ticketId
	 * @return
	 */
	public static String resolveTicketByAdmin(final Long ticketId) {
		StringBuilder message = new StringBuilder();
		message.append("Your raised Ticket ").append(ticketId.toString()).append(" is resolved by admin, You can check the more details  in ticket section");
		return message.toString();
	}


	/**
	 *
	 * @param orderId
	 * @param amount
	 * @return
	 */
	public static String getRefundOrderMessage(final Long orderId, final Double amount) {
		StringBuilder message = new StringBuilder();
		message.append("Your refund for ").append(amount).append(" against order no. ").append(orderId).append(" has credited to your Nice wallet");
		return message.toString();
	}

	/**
	 *
	 * @return
	 */
	public static String profileSuspendedForCustomer() {
		StringBuilder message = new StringBuilder();
		message.append("Your Customer account on ").append("Nice").append(" has been temporarily  suspended for violating the terms and conditions of ")
				.append("Nice");
		return message.toString();
	}

	/**
	 * @param orderId
	 * @param concat
	 * @return
	 */
	public static String getOrderAcceptedMessageToCustomer(final Long orderId, final String deliveryBoyName) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" is Accepted by delivery boy ").append(deliveryBoyName);
		return message.toString();
	}

}
