package com.nice.service;

import java.util.Map;

/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Oct-2020
 */
public interface SocketIOService {
	/**
	 * Start Services
	 */
	void start();

	/**
	 * Out of Service
	 */
	void stop();

	/**
	 * Push information to specified client
	 *
	 * @param orderId:    Order/Client Unique Identification
	 * @param msgContent: Message Content
	 */
	void pushMessageToUser(String orderId, Map<String, String> msgContent);

}
