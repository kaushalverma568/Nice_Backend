package com.nice.service;

import com.nice.dto.OrderLocationDTO;

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
	void pushMessageToUser(String orderId, OrderLocationDTO msgContent);
}
