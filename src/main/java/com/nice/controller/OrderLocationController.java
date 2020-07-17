package com.nice.controller;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.nice.dto.OrderLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.OrderLocationService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
@RestController
@RequestMapping(path = "/order/location")
public class OrderLocationController {

	private static final String LATITUDE = "latitude";
	private static final String LONGITUDE = "longitude";
	private static final String DELIVERY_BOY_ID = "deliveryBoyId";
	private static final String ORDER_ID = "orderId";
	private static final String CUSTOMER_ID = "customerId";
	private static final Logger LOGGER = LoggerFactory.getLogger(OrderLocationController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrderLocationService orderLocationService;

	@Autowired
	private SimpMessagingTemplate simpMessagingTemplate;

	@SuppressWarnings("unchecked")
	@MessageMapping("/send/message")
	public ResponseEntity<Object> addDeliveryBoyLocationViaSocket(final String message) {
		ObjectMapper mapper = new ObjectMapper();
		Map<String, String> messageConverted = null;
		try {
			messageConverted = mapper.readValue(message, Map.class);
		} catch (IOException e) {
			messageConverted = null;
		}
		if (validateSocketMessage(messageConverted)) {
			OrderLocationDTO locationDTO = new OrderLocationDTO();
			locationDTO.setDeliveryBoyId(Long.parseLong(messageConverted.get(DELIVERY_BOY_ID)));
			locationDTO.setCustomerId(Long.parseLong(messageConverted.get(CUSTOMER_ID)));
			locationDTO.setOrderId(Long.parseLong(messageConverted.get(ORDER_ID)));
			locationDTO.setLatitude(new BigDecimal(messageConverted.get(LATITUDE)));
			locationDTO.setLongitude(new BigDecimal(messageConverted.get(LONGITUDE)));
			try {
				orderLocationService.addOrderLocation(locationDTO);
				simpMessagingTemplate.convertAndSend("/socket-publisher/" + "deliveryBoy".concat(locationDTO.getDeliveryBoyId().toString()), message);
				simpMessagingTemplate.convertAndSend("/socket-publisher/" + "customer".concat(locationDTO.getCustomerId().toString()), message);
			} catch (ValidationException e) {
				LOGGER.info("valiation error occur while add location with message : {}", e.getMessage());
			} catch (NotFoundException e) {
				LOGGER.info("not found error occur while add location with message : {}", e.getMessage());
			}
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("deliveryboy.location.create.message", null)).create();
	}

	private boolean validateSocketMessage(final Map<String, String> messageConverted) {
		return messageConverted != null && messageConverted.containsKey(LATITUDE) && messageConverted.get(LATITUDE) != null
				&& messageConverted.containsKey(LONGITUDE) && messageConverted.get(LONGITUDE) != null && messageConverted.containsKey(DELIVERY_BOY_ID)
				&& messageConverted.get(DELIVERY_BOY_ID) != null && messageConverted.containsKey(CUSTOMER_ID) && messageConverted.get(CUSTOMER_ID) != null
				&& messageConverted.containsKey(ORDER_ID) && messageConverted.get(ORDER_ID) != null;
	}
}
