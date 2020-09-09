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
	private static Double orderDeliveryChargeCustomer = 0.0d;
	private static Double orderDeliveryChargeDeliveryBoy = 0.0d;
	private static Double fixedDeliveryChargeCustomer = 0.0d;
	private static Double fixedDeliveryChargeDeliveryBoy = 0.0d;
	private static Double minimumKMForDeliveryCharge = 0.0d;

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
			orderDeliveryChargeCustomer = Double.valueOf(String.valueOf(value));
		} else if ("ORDER_DELIVERY_CHARGE_DELIVERY_BOY".equalsIgnoreCase(key)) {
			orderDeliveryChargeDeliveryBoy = Double.valueOf(String.valueOf(value));
		} else if ("FIXED_DELIVERY_CHARGE_CUSTOMER".equalsIgnoreCase(key)) {
			fixedDeliveryChargeCustomer = Double.valueOf(String.valueOf(value));
		} else if ("FIXED_DELIVERY_CHARGE_DELIVERY_BOY".equalsIgnoreCase(key)) {
			fixedDeliveryChargeDeliveryBoy = Double.valueOf(String.valueOf(value));
		} else if ("MINIMUM_KM_FOR_DELIVERY_CHARGE".equalsIgnoreCase(key)) {
			minimumKMForDeliveryCharge = Double.valueOf(String.valueOf(value));
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
			return orderDeliveryChargeCustomer;
		} else if ("ORDER_DELIVERY_CHARGE_DELIVERY_BOY".equalsIgnoreCase(key)) {
			return orderDeliveryChargeDeliveryBoy;
		} else if ("FIXED_DELIVERY_CHARGE_CUSTOMER".equalsIgnoreCase(key)) {
			return fixedDeliveryChargeCustomer;
		} else if ("FIXED_DELIVERY_CHARGE_DELIVERY_BOY".equalsIgnoreCase(key)) {
			return fixedDeliveryChargeDeliveryBoy;
		} else if ("MINIMUM_KM_FOR_DELIVERY_CHARGE".equalsIgnoreCase(key)) {
			return minimumKMForDeliveryCharge;
		}
		return null;
	}

}
