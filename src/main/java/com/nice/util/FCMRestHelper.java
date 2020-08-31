/**
 *
 */
package com.nice.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.web.client.RestTemplate;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 10-Jul-2020
 */
public class FCMRestHelper {

	/**
	 *
	 */
	private static final String TOPICS = "/topics/";

	/**
	 * Instance
	 **/

	private static Map<String, FCMRestHelper> instanceMap = new HashMap<>();

	/**
	 * Google URL to use firebase cloud messenging
	 */
	private static final String FCM_URL = "https://fcm.googleapis.com/fcm/send";

	/**
	 * STATIC TYPES
	 */

	public static final String TYPE_TO = "to"; // Use for single devices and topics
	public static final String TYPE_CONDITION = "condition"; // Use for Conditions
	public static final String TYPE_REGISTRATION = "registration_ids";

	/**
	 * Your SECRET server key
	 */
	private static final String FCM_SERVER_KEY = "AAAAdaCL7Gg:APA91bEn8ZGna_WMN6EdFM8u9s9nIRrxRu_Wp31BiOtsuzy3hBnFFPBgUM0O_bAOHXjIQqWkOV-H1DKXI6rSfStcXnLuCT5dCyjSC4UjOe9tcxKuqsXHmI3mGMtaybthyNr_3RLFZlQo";

	public static FCMRestHelper getInstance(final String fcmKey) {

		if (instanceMap.get(fcmKey) == null) {
			FCMRestHelper instance = new FCMRestHelper();
			instanceMap.put(fcmKey, instance);
		}
		return instanceMap.get(fcmKey);
	}

	private FCMRestHelper() {
	}

	/**
	 * Send notification
	 *
	 * @param  type
	 * @param  typeParameter
	 * @param  notificationObject
	 * @return
	 */
	public String sendNotification(final String type, final String typeParameter, final JsonObject notificationObject) {
		return sendNotifictaionAndData(type, typeParameter, notificationObject, null);
	}

	/**
	 * Send data
	 *
	 * @param  type
	 * @param  typeParameter
	 * @param  dataObject
	 * @return
	 */
	public String sendData(final String type, final String typeParameter, final JsonObject dataObject) {
		return sendNotifictaionAndData(type, typeParameter, null, dataObject);
	}

	/**
	 * Send data on multiple devices
	 *
	 * @param  type
	 * @param  typeParameter
	 * @param  dataObject
	 * @return
	 */
	public String sendDataMulti(final String type, final List<String> recipientList, final JsonObject dataObject) {
		return sendNotifictaionAndDataMulti(type, recipientList, null, dataObject);
	}

	/**
	 * Send notification and data
	 *
	 * @param  type
	 * @param  typeParameter
	 * @param  notificationObject
	 * @param  dataObject
	 * @return
	 */
	public String sendNotifictaionAndData(final String type, final String typeParameter, final JsonObject notificationObject, final JsonObject dataObject) {
		String result = null;
		if (type.equals(TYPE_TO) || type.equals(TYPE_CONDITION)) {
			JsonObject sendObject = new JsonObject();
			sendObject.addProperty(type, typeParameter);
			result = sendFcmMessage(sendObject, notificationObject, dataObject);
		}
		return result;
	}

	/**
	 * Send notification and data on multiple devices
	 *
	 * @param  type
	 * @param  recipientList
	 * @param  notificationObject
	 * @param  dataObject
	 * @return
	 */
	public String sendNotifictaionAndDataMulti(final String type, final List<String> recipientList, final JsonObject notificationObject,
			final JsonObject dataObject) {
		String result = null;
		if (type.equals(TYPE_REGISTRATION)) {
			JsonObject sendObject = new JsonObject();
			JsonArray jsonArray = new JsonArray();
			for (String string : recipientList) {
				jsonArray.add(string);
			}
			sendObject.add(type, jsonArray);
			result = sendFcmMessage(sendObject, notificationObject, dataObject);
		}
		return result;
	}

	/**
	 * Send data to a topic
	 *
	 * @param  topic
	 * @param  dataObject
	 * @return
	 */
	public String sendTopicData(final String topic, final JsonObject dataObject) {
		return sendData(TYPE_TO, TOPICS + topic, dataObject);
	}

	/**
	 * Send notification to a topic
	 *
	 * @param  topic
	 * @param  notificationObject
	 * @return
	 */
	public String sendTopicNotification(final String topic, final JsonObject notificationObject) {
		return sendNotification(TYPE_TO, TOPICS + topic, notificationObject);
	}

	/**
	 * Send notification and data to a topic
	 *
	 * @param  topic
	 * @param  notificationObject
	 * @param  dataObject
	 * @return
	 */
	public String sendTopicNotificationAndData(final String topic, final JsonObject notificationObject, final JsonObject dataObject) {
		return sendNotifictaionAndData(TYPE_TO, TOPICS + topic, notificationObject, dataObject);
	}

	/**
	 * Send a Firebase Cloud Message
	 *
	 * @param  sendObject         - Contains to or condition
	 * @param  notificationObject - Notification Data
	 * @param  dataObject         - Data
	 * @return
	 */
	private String sendFcmMessage(final JsonObject sendObject, final JsonObject notificationObject, final JsonObject dataObject) {
		RestTemplate restTemplate = new RestTemplate();
		HttpHeaders httpHeaders = new HttpHeaders();

		httpHeaders.set("Content-Type", "application/json");
		httpHeaders.set("Authorization", "key=" + FCM_SERVER_KEY);

		if (notificationObject != null) {
			sendObject.add("notification", notificationObject);
		}
		if (dataObject != null) {
			sendObject.add("data", dataObject);
		}

		HttpEntity<String> httpEntity = new HttpEntity<>(sendObject.toString(), httpHeaders);
		return restTemplate.postForObject(FCM_URL, httpEntity, String.class);

	}

}
