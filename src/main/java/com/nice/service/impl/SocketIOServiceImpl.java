package com.nice.service.impl;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.corundumstudio.socketio.SocketIOClient;
import com.corundumstudio.socketio.SocketIOServer;
import com.nice.dto.OrderLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.service.OrderLocationService;
import com.nice.service.SocketIOService;

@Service(value = "socketIOService")
public class SocketIOServiceImpl implements SocketIOService {
	private static final Logger LOGGER = LoggerFactory.getLogger(SocketIOServiceImpl.class);

	@Autowired
	private OrderLocationService orderLocationService;
	/**
	 * Store connected orders
	 */
	private static Map<String, SocketIOClient> clientMap = new ConcurrentHashMap<>();

	/**
	 * Custom Event`push_data_event` for service side to client communication
	 */
	private static final String PUSH_DATA_EVENT = "push_data_event";

	@Autowired
	private SocketIOServer socketIOServer;

	/**
	 * Spring IoC After the container is created, start after loading the
	 * SocketIOServiceImpl Bean
	 */
	@PostConstruct
	private void autoStartup() {
		start();
	}

	/**
	 * Spring IoC Container closes before destroying SocketIOServiceImpl Bean to
	 * avoid restarting project service port occupancy
	 */
	@PreDestroy
	private void autoStop() {
		stop();
	}

	@Override
	public void start() {
		// Listen for client connections
		socketIOServer.addConnectListener(client -> {
			LOGGER.debug("************ Client: ".concat(getIpByClient(client)).concat(" Connected ************"));
			// Custom Events `connected` -> communicate with clients (built-in events such
			// as Socket.EVENT_CONNECT can also be used)
			client.sendEvent("connected", "You're connected successfully...");
			String orderId = getParamsByClient(client);
			if (orderId != null) {
				clientMap.put(orderId, client);
			}
		});

		// Listening Client Disconnect
		socketIOServer.addDisconnectListener(client -> {
			String clientIp = getIpByClient(client);
			LOGGER.debug(clientIp.concat(" *********************** Client disconnected"));
			String orderId = getParamsByClient(client);
			if (orderId != null) {
				clientMap.remove(orderId);
				client.disconnect();
			}
		});

		// Custom Event`client_info_event` ->Listen for client messages
		socketIOServer.addEventListener(PUSH_DATA_EVENT, OrderLocationDTO.class, (client, data, ackSender) -> {
			// When a client pushes a `client_info_event` event, onData accepts data, which
			// is json data of type string here and can
			// be Byte[], other types of object
			if (validateSocketMessage(data)) {
				try {
					orderLocationService.addOrderLocation(data);
					pushMessageToUser(data.getOrderId().toString() + ' ' + data.getCustomerId().toString() + "_receiver", data);
					pushMessageToUser(data.getOrderId().toString() + ' ' + data.getDeliveryBoyId().toString() + "_sender", data);
				} catch (ValidationException e) {
					LOGGER.info("valiation error occur while add location with message : {}", e.getMessage());
				} catch (NotFoundException e) {
					LOGGER.info("not found error occur while add location with message : {}", e.getMessage());
				}
			}
		});

		// Start Services
		socketIOServer.start();
	}

	@Override
	public void stop() {
		if (socketIOServer != null) {
			socketIOServer.stop();
			socketIOServer = null;
		}
	}

	@Override
	public void pushMessageToUser(final String orderId, final OrderLocationDTO msgContent) {
		SocketIOClient client = clientMap.get(orderId);
		if (client != null) {
			client.sendEvent(PUSH_DATA_EVENT, msgContent);
		}
	}

	/**
	 * Get the userId parameter in the client url (modified here to suit individual
	 * needs and client side)
	 *
	 * @param client: Client
	 * @return: java.lang.String
	 */
	private String getParamsByClient(final SocketIOClient client) {
		// Get the client url parameter (where userId is the unique identity)
		Map<String, List<String>> params = client.getHandshakeData().getUrlParams();
		List<String> userIdList = params.get("orderId");
		if (!userIdList.isEmpty()) {
			return userIdList.get(0);
		}
		return null;
	}

	/**
	 * Get the connected client ip address
	 *
	 * @param client: Client
	 * @return: java.lang.String
	 */
	private String getIpByClient(final SocketIOClient client) {
		String sa = client.getRemoteAddress().toString();
		return sa.substring(1, sa.indexOf(":"));
	}

	private boolean validateSocketMessage(final OrderLocationDTO orderLocationDTO) {
		return orderLocationDTO != null && orderLocationDTO.getCustomerId() != null && orderLocationDTO.getDeliveryBoyId() != null
				&& orderLocationDTO.getLatitude() != null && orderLocationDTO.getLongitude() != null && orderLocationDTO.getOrderId() != null;
	}

}
