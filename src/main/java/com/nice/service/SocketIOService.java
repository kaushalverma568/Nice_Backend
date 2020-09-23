package com.nice.service;

import java.util.Map;

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
	 * @param userId:     Client Unique Identification
	 * @param msgContent: Message Content
	 */
	void pushMessageToUser(String orderId, Map<String, String> msgContent);

}
