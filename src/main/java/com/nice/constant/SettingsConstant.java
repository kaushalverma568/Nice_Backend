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
	/**
	 * This contains the minimum orders to be delivered per day to be eligible for increased delivery charge
	 */
	private static Double dayMinOrderDelivered = 0.0d;
	private static Double orderAmountForFreeDelivery = 0.0d;
	private static Double adminComission = 0.0d;

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
		} else if (Constant.ORDER_DELIVERY_CHARGE.equalsIgnoreCase(key)) {
			orderDeliveryCharge = Double.valueOf(String.valueOf(value));
		} else if (Constant.DAY_MIN_ORDER_DELIVERED.equalsIgnoreCase(key)) {
			dayMinOrderDelivered = Double.valueOf(String.valueOf(value));
		} else if (Constant.ORDER_AMOUNT_FOR_FREE_DELIVERY.equalsIgnoreCase(key)) {
			orderAmountForFreeDelivery = Double.valueOf(String.valueOf(value));
		} else if (Constant.ADMIN_COMISSION.equalsIgnoreCase(key)) {
			adminComission = Double.valueOf(String.valueOf(value));
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
		} else if (Constant.ORDER_DELIVERY_CHARGE.equalsIgnoreCase(key)) {
			return orderDeliveryCharge;
		} else if (Constant.DAY_MIN_ORDER_DELIVERED.equalsIgnoreCase(key)) {
			return dayMinOrderDelivered;
		} else if (Constant.ORDER_AMOUNT_FOR_FREE_DELIVERY.equalsIgnoreCase(key)) {
			return orderAmountForFreeDelivery;
		} else if (Constant.ADMIN_COMISSION.equalsIgnoreCase(key)) {
			return adminComission;
		}
		return null;
	}

}
