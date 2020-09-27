package com.nice.service.impl;

import java.io.IOException;
import java.math.BigDecimal;
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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nice.dto.OrderLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrderLocation;
import com.nice.service.OrderLocationService;
import com.nice.service.SocketIOService;
import com.nice.util.JsonObjectMapper;

@Service(value = "socketIOService")
public class SocketIOServiceImpl implements SocketIOService {
	private static final Logger LOGGER = LoggerFactory.getLogger(SocketIOServiceImpl.class);
	private static final String LATITUDE = "latitude";
	private static final String LONGITUDE = "longitude";
	private static final String DELIVERY_BOY_ID = "deliveryBoyId";
	private static final String ORDER_ID = "orderId";
	private static final String CUSTOMER_ID = "customerId";
	@Autowired
	private OrderLocationService orderLocationService;
	/**
	 * Store connected orders
	 */
	private static Map<String, SocketIOClient> connectedClients = new ConcurrentHashMap<>();

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

	@SuppressWarnings("unchecked")
	@Override
	public void start() {
		// Listen for client connections
		socketIOServer.addConnectListener(order -> {
			LOGGER.debug("************ Client: ".concat(getIpByClient(order)).concat(" Connected ************"));
			// Custom Events `connected` -> communicate with clients (built-in events such
			// as Socket.EVENT_CONNECT can also be used)
			order.sendEvent("connected", "You're connected successfully...");
			String orderId = getParamsByClient(order);
			if (orderId != null) {
				connectedClients.put(orderId, order);
			}
		});

		// Listening Client Disconnect
		socketIOServer.addDisconnectListener(order -> {
			String clientIp = getIpByClient(order);
			LOGGER.debug(clientIp.concat(" *********************** Client disconnected"));
			String orderId = getParamsByClient(order);
			if (orderId != null) {
				connectedClients.remove(orderId);
				order.disconnect();
			}
		});

		// Custom Event`client_info_event` ->Listen for client messages
		socketIOServer.addEventListener(PUSH_DATA_EVENT, String.class, (client, data, ackSender) -> {
			ObjectMapper mapper = JsonObjectMapper.getInstance().getObjectMapper();
			Map<String, String> messageConverted = null;
			try {
				messageConverted = mapper.readValue(data, Map.class);
			} catch (IOException e) {
				messageConverted = null;
			}
			if (validateSocketMessage(messageConverted)) {
				try {
					OrderLocationDTO locationDTO = new OrderLocationDTO();
					locationDTO.setDeliveryBoyId(Long.parseLong(messageConverted.get(DELIVERY_BOY_ID)));
					locationDTO.setOrderId(Long.parseLong(messageConverted.get(ORDER_ID)));
					locationDTO.setLatitude(new BigDecimal(messageConverted.get(LATITUDE)));
					locationDTO.setLongitude(new BigDecimal(messageConverted.get(LONGITUDE)));
					OrderLocation orderLocation = orderLocationService.addOrderLocation(locationDTO);
					/**
					 * here for customer orderId will be orderId_customerId_receiver and for
					 * delivery Boy orderId will be orderId_deliveryBoyId_sender
					 */
					messageConverted.put(CUSTOMER_ID, orderLocation.getCustomerId().toString());
					pushMessageToUser(locationDTO.getOrderId().toString().concat("_").concat(locationDTO.getCustomerId().toString()).concat("_receiver"),
							messageConverted);
					pushMessageToUser(locationDTO.getOrderId().toString().concat("_").concat(locationDTO.getDeliveryBoyId().toString()).concat("_sender"),
							messageConverted);
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
	public void pushMessageToUser(final String orderId, final Map<String, String> msgContent) {
		SocketIOClient client = connectedClients.get(orderId);
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
		// Get the client url parameter (where orderId is the unique identity)
		Map<String, List<String>> params = client.getHandshakeData().getUrlParams();
		List<String> userIdList = params.get(ORDER_ID);
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

	private boolean validateSocketMessage(final Map<String, String> messageConverted) {
		return messageConverted != null && messageConverted.containsKey(LATITUDE) && messageConverted.get(LATITUDE) != null
				&& messageConverted.containsKey(LONGITUDE) && messageConverted.get(LONGITUDE) != null && messageConverted.containsKey(DELIVERY_BOY_ID)
				&& messageConverted.get(DELIVERY_BOY_ID) != null && messageConverted.containsKey(ORDER_ID) && messageConverted.get(ORDER_ID) != null;
	}
}
