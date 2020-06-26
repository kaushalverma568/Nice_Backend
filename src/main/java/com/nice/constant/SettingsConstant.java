/**
 *
 */
package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
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
		}
	}

	/**
	 * @param  key
	 * @return
	 */
	public static String getSettingsValue(final String key) {
		if ("SEND_SMS".equalsIgnoreCase(key)) {
			return sendSMS;
		} else if ("SEND_EMAIL".equalsIgnoreCase(key)) {
			return sendEmail;
		} else if ("SMS_API_KEY".equalsIgnoreCase(key)) {
			return smsApiKey;
		}
		return null;
	}

}
