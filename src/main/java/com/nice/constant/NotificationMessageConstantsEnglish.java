package com.nice.constant;

import java.time.ZoneId;
import java.util.Date;
import java.util.Map;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public final class NotificationMessageConstantsEnglish {

	private NotificationMessageConstantsEnglish() {
		super();
	}

	public static final String DELIVERY_BOY_PAYOUT_SUBJECT = "Delivery Boy Payment";
	public static final String VENDOR_PAYOUT_SUBJECT = "Vendor Payment";
	public static final String ACCOUNT_ACCTIVATION_SUBJECT = "Account Activation";
	public static final String USER_TYPE_CUSTOMER = "Customer";
	public static final String USER_TYPE_DELIVERY_BOY = "Delivery Boy";
	public static final String USER_TYPE_USER = "User";
	public static final String USER_TYPE_VENDOR = "Vendor";
	public static final String EMAIL_VERIFICATION_SUBJECT = "Email Verification Alert!";
	public static final String THANKS_REGARDS = "Thanks & regards,";
	public static final String CUSTOMER_SUPPORT_TEAM = "customer support team";
	public static final String CUSTOMER_CARE_NO = "Customer care no.";
	public static final String EMAIL_ADDRESS = "Email address";
	public static final String HELLO = "Hello";
	public static final String DEAR = "Dear";
	public static final String WELCOME = "Welcome";
	public static final String OR = "OR";

	public static final Map<String, String> orderStatusMap = Map.of(OrderStatusEnum.DELIVERED.getStatusValue(), "Delivered",
			OrderStatusEnum.REJECTED.getStatusValue(), "Rejected", OrderStatusEnum.REPLACED.getStatusValue(), "Replaced", "RETURN", "Return", "Replace",
			"Replace");

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
	public static String getNormalOrderAcceptMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have new order Request to accept with order no. ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getReturnOrderAcceptMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have new Return order Request to accept with order no. ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getReplaceOrderAcceptMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append(" You have new Replacement order Request to accept with order no. ").append(orderId.toString());
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
	 * @param  name
	 * @return
	 */
	public static String regularOrderIsPreparedMessageToDeliveryBoy(final String vendorName, final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("Your Regular order no. ").append(orderId).append(" is prepared,Please collect from the vendor ").append(vendorName);
		return message.toString();
	}

	/**
	 * @param  name
	 * @return
	 */
	public static String replaceOrderIsPreparedMessageToDeliveryBoy(final String vendorName, final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("Your Replacement order no. ").append(orderId).append(" is prepared,Please collect from the vendor ").append(vendorName);
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getNewOrderToVendorMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("You have received one new cart order with order no. ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  name
	 * @param  orderId
	 * @return
	 */
	public static String getDeliveryOrderToVendorMessage(final String name, final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append(name).append(" has delivered an order no. ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  name
	 * @param  orderId
	 * @return
	 */
	public static String getPayoutMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Your payment is release by admin, Kindly check your registered bank account for more details");
		return message.toString();
	}

	public static String getPayoutMessage(final Double amount, final Date paidOn) {
		StringBuilder message = new StringBuilder();
		message = message.append("Your payment for KD ").append(amount).append(" has been processed on ")
				.append(paidOn.toInstant().atZone(ZoneId.systemDefault()).toLocalDate().toString())
				.append(". Kindly check your registered bank account for the same");
		return message.toString();
	}

	public static String getPayoutSecondMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Kindly check your registered bank account for the same.");
		return message.toString();
	}

	public static String resumeVendorProfileMessage(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append("This is to notify you that your vendor account on ").append(applicationName)
				.append(" has been resumed from our side. Now you are able to use our panel services.");
		return message.toString();
	}

	public static String resumeVendorProfileSecondMessage() {
		StringBuilder message = new StringBuilder();
		message.append("Please contact customer care for further information.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  orderAmount
	 * @return
	 */
	public static String getCreateOrderMessage(final Long orderId, final Double orderAmount) {
		StringBuilder message = new StringBuilder();
		message.append("Congratulations! Your order no.").append(orderId).append(" for amount ").append(orderAmount).append(" has been placed successfully.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getCancelOrderMessage(final Long orderId, final boolean cancelByCustomer) {
		StringBuilder message = new StringBuilder();
		if (cancelByCustomer) {
			message.append("You have cancelled an order no. ").append(orderId).append(". If it is cancelled by mistake please place the order again.");
		} else {
			message.append("Your order no. ").append(orderId).append("is cancelled by admin, sorry for disappointment.");
		}

		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String getOrderStatusUpdateMessageExceptDelivery(final Long orderId, final String currentStatus) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" is ").append(currentStatus).append(". We will delivery it shortly.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String orderDeliverySuccessful(final Long orderId, final String orderStatus) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" has been sucessfully ").append(orderStatus).append(".");
		if (OrderStatusEnum.DELIVERED.getStatusValue().equals(orderStatus)) {
			message.append(" Kindly help us to improve our service by giving your feedback!");
		}
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String orderItemReplaceSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your request to replace order item/s for order no. ").append(orderId).append(" has been placed successfully.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String orderItemReturnSuccessful(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your request to return order item/s for order no. ").append(orderId).append(" has been placed successfully.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  orderAmount
	 * @return
	 */
	public static String getRejectedOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Sorry! Your order no. ").append(orderId)
				.append(" has been rejected by vendor. If you have paid for the order online the amount would be refunded your nice wallet");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String replaceRequestInitiated(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have received replacement request for the order no. : ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String returnRequestInitiated(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("You have received return request for the order no. : ").append(orderId.toString());
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @return
	 */
	public static String cancelOrderByAdmin(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId.toString()).append(" has cancel by admin,Sorry for disappointment");
		return message.toString();
	}

	public static String cancelOrderByAdminForDeliveryBoy(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message = message.append("Your accepted order is cancel by the admin with order no. ").append(orderId);
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
	 * @return
	 */
	public static String profileSuspendedForCustomer() {
		StringBuilder message = new StringBuilder();
		message.append("Your Customer account on Nice has been temporarily  suspended for violating the terms and conditions of Nice");
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
				"Nice team has reviewed and approved your profile. Now, you are a part of the Nice Application. Log in to the system and get full access to the Nice Application.");
		return message.toString();
	}

	public static String rejectVendorProfileMessage() {
		StringBuilder message = new StringBuilder();
		message.append("Nice has Review Your profile, your profile is not approved by our team sorry for inconvenience.");
		return message.toString();
	}

	public static String rejectVendorProfileSecondMessage() {
		StringBuilder message = new StringBuilder();
		message.append("For further information you can contact to our customer care.");
		return message.toString();
	}

	public static String suspendVendorProfileMessage() {
		StringBuilder message = new StringBuilder();
		message.append(
				"This is to notify you that your vendor account on Nice Application has been temporarily  suspended for violating the terms and conditions of Nice Application.");
		return message.toString();
	}

	public static String suspendVendorProfileSecondMessage() {
		StringBuilder message = new StringBuilder();
		message.append("Please the contact to customer care for further information and You will be notified once your account will be resume");
		return message.toString();
	}

	public static String vendorSubscriptionExpiredMessage(final Double amount, final Date expiredDate) {
		StringBuilder message = new StringBuilder();
		message.append("You Nice Application subscription plan of KD").append(amount.toString()).append(" has expired on ").append(expiredDate)
				.append(". To Continue enjoying  Nice Application services, renew the subscription plan, or purchase a new subscription.\r\n" + "\r\n"
						+ "To renew or make a purchase of subscription plan click ");
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
		message.append("Your Nice account is activated by admin, Now you can Login-in into the Nice delivery boy application.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String placeOrderMessage(final Long orderId, final Double amount) {
		StringBuilder message = new StringBuilder();
		message.append("Your Order with order no. ").append(orderId).append(" for amount ").append(amount)
				.append(" has been received by the vendor. It will be processed shortly.");
		return message.toString();
	}

	public static String placeOrderSubject(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Nice - Placed Order - ").append(orderId);
		return message.toString();
	}

	public static String returnOrderSubject(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Nice - Return Order - ").append(orderId);
		return message.toString();
	}

	public static String cancelOrderSubject(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Nice - Cancel Order - ").append(orderId);
		return message.toString();
	}

	public static String replacementOrderSubject(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Nice - Replacement Order - ").append(orderId);
		return message.toString();
	}

	public static String deliveryOrderSubject(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Nice - Delivered Order - ").append(orderId);
		return message.toString();
	}

	public static String retryToSearchDeliveryBoy(final Long orderId, final String vendorName) {
		StringBuilder message = new StringBuilder();
		message = message.append("Dear Vendor ").append(vendorName).append(" System couldn't found the near by delivery boy for order no. ").append(orderId);
		message.append(", Please Retry the Request");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String thankYouForShopping() {
		StringBuilder message = new StringBuilder();
		message.append("Thank you for shopping with us!");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String returnOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your return request for order no. ").append(orderId).append(" has been received. We will process return promptly.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String cancelOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your order no. ").append(orderId).append(" has been cancelled. If it is cancelled by mistake, please place the order again.")
				.append("If you have paid the amount online it would be refunded in your Nice wallet. You can use this amount for next purchase.");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  concat
	 * @return
	 */
	public static String getOrderAcceptedMessageToCustomer(final Long orderId, final String deliveryBoyName, final String orderStatus) {
		StringBuilder message = new StringBuilder();
		if (orderStatus.equals("DELIVERY")) {
			message.append("Your").append(" order no. ").append(orderId).append(" is Accepted by delivery boy ").append(deliveryBoyName);
		} else {
			message.append("Your ").append(orderStatusMap.get(orderStatus)).append(" order no. ").append(orderId).append(" is Accepted by delivery boy ")
					.append(deliveryBoyName);
		}

		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String getRefundOrderMessage(final Long orderId, final Double amount) {
		StringBuilder message = new StringBuilder();
		message.append("Your refund for ").append(amount).append(" against order no. ").append(orderId).append(" has credited to your Nice wallet");
		return message.toString();
	}

	/**
	 * @param  orderId
	 * @param  amount
	 * @return
	 */
	public static String replaceOrderMessage(final Long orderId) {
		StringBuilder message = new StringBuilder();
		message.append("Your replacement request for order no. ").append(orderId)
				.append(" has been successfully placed. We will process replacement promptly.");
		return message.toString();
	}

	public static String welcomeSubject(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append("Welcome to the ").append(applicationName);
		return message.toString();
	}

	public static String resetPasswordSubject(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append(applicationName).append("- Reset Password");
		return message.toString();
	}

	public static String welcome(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append("Thank you for being a part of the Nice community\r\n"
				+ "You can now order food and groceries online on Nice application from vendors that may interest you. Enjoy shopping without hassle from anywhere with Nice.");
		return message.toString();
	}

	public static String welcomeSecondMessage() {
		StringBuilder message = new StringBuilder();
		message.append("Feel our customer support beside you for any queries!");
		return message.toString();
	}

	public static String welcomeVendor(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append("Welcome to ").append(applicationName).append("!\r\n").append(
				"We are pleased to have you as a partner and looking forward to working with you as a team.\r\nKindly update your store and necessary details on the ")
				.append(applicationName).append(" platform to let our users notice and order.");
		return message.toString();
	}

	public static String getOtpValidityMessage(final Long otpValidity) {
		StringBuilder message = new StringBuilder();
		message = message.append("You can use below OTP which is valid only for the next ").append(otpValidity).append(" minutes.");
		return message.toString();
	}

	public static String getLinkValidityMessage(final Long otpValidity) {
		StringBuilder message = new StringBuilder();
		message = message.append("This link is valid till next ").append(otpValidity).append(" minutes.");
		return message.toString();
	}

	public static String sendOtpSubject(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append(applicationName).append(" : OTP for verification");
		return message.toString();
	}

	public static String subscriptionExpireSubject(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message.append(applicationName).append(" Subscription Expire Reminder");
		return message.toString();
	}

	public static String getInsructionMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Do not share this OTP to anyone for security reasons.");
		return message.toString();
	}

	public static String getInsructionMessageForLink() {
		StringBuilder message = new StringBuilder();
		message = message.append("Do not share this link to anyone for security reasons.");
		return message.toString();
	}

	public static String getResetPasswordInsructionMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Do not ignore this message or else your password will not be changed.");
		return message.toString();
	}

	public static String getOTPMessage() {
		StringBuilder message = new StringBuilder();
		message = message.append("Your one time password to proceed with verification");
		return message.toString();
	}

	public static String welcomeDeliveryBoy(final String applicationName) {
		StringBuilder message = new StringBuilder();
		message = message.append("You are now part of a ").append(applicationName)
				.append(" community. Together we will deliver a pleasant experience to our customers with their order.");
		return message.toString();
	}
}
