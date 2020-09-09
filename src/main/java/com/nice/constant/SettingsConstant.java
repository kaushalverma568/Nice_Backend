/**
 *
 */
package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
public final class SettingsConstant {

	/**
	 *
	 */
	private SettingsConstant() {
		super();
	}

	private static String sendSMS = "false";
	private static String sendEmail = "false";
	private static String smsApiKey = "default";
	private static Double orderDeliveryCharge = 0.0d;
	private static Double deliveryChargeDeliveryBoyBelowMinOrders = 0.0d;
	private static Double deliveryChargeDeliveryBoyAboveMinOrders = 0.0d;
	/**
	 * This contains the minimum orders to be delivered per day to be eligible for increased delivery charge
	 */
	private static Double dayMinOrderDelivered = 0.0d;
	private static Double orderAmountForFreeDelivery = 0.0d;

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
		} else if ("ORDER_DELIVERY_CHARGE".equalsIgnoreCase(key)) {
			orderDeliveryCharge = Double.valueOf(String.valueOf(value));
		} else if ("DELIVERY_CHARGE_DELIVERY_BOY_BELOW_MIN_ORDERS".equalsIgnoreCase(key)) {
			deliveryChargeDeliveryBoyBelowMinOrders = Double.valueOf(String.valueOf(value));
		} else if ("DELIVERY_CHARGE_DELIVERY_BOY_ABOVE_MIN_ORDERS".equalsIgnoreCase(key)) {
			deliveryChargeDeliveryBoyAboveMinOrders = Double.valueOf(String.valueOf(value));
		} else if ("DAY_MIN_ORDER_DELIVERED".equalsIgnoreCase(key)) {
			dayMinOrderDelivered = Double.valueOf(String.valueOf(value));
		} else if ("ORDER_AMOUNT_FOR_FREE_DELIVERY".equalsIgnoreCase(key)) {
			orderAmountForFreeDelivery = Double.valueOf(String.valueOf(value));
		}
	}

	/**
	 * @param key
	 * @return
	 */
	public static Object getSettingsValue(final String key) {
		if ("SEND_SMS".equalsIgnoreCase(key)) {
			return sendSMS;
		} else if ("SEND_EMAIL".equalsIgnoreCase(key)) {
			return sendEmail;
		} else if ("SMS_API_KEY".equalsIgnoreCase(key)) {
			return smsApiKey;
		} else if ("ORDER_DELIVERY_CHARGE".equalsIgnoreCase(key)) {
			return orderDeliveryCharge;
		} else if ("DELIVERY_CHARGE_DELIVERY_BOY_BELOW_MIN_ORDERS".equalsIgnoreCase(key)) {
			return deliveryChargeDeliveryBoyBelowMinOrders;
		} else if ("DELIVERY_CHARGE_DELIVERY_BOY_ABOVE_MIN_ORDERS".equalsIgnoreCase(key)) {
			return deliveryChargeDeliveryBoyAboveMinOrders;
		} else if ("DAY_MIN_ORDER_DELIVERED".equalsIgnoreCase(key)) {
			return dayMinOrderDelivered;
		} else if ("ORDER_AMOUNT_FOR_FREE_DELIVERY".equalsIgnoreCase(key)) {
			return orderAmountForFreeDelivery;
		}
		return null;
	}

}
