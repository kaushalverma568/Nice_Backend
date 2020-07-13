package com.nice.service.impl;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.Constant;
import com.nice.model.OnlineRequest;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.service.OnlineService;
import com.nice.service.SettingsService;
import com.razorpay.Order;
import com.razorpay.RazorpayClient;
import com.razorpay.RazorpayException;

/**
 * 
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 13-07-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("onlineService")
public class OnlineServiceImpl implements OnlineService {

	private static final Logger LOGGER = LoggerFactory.getLogger(OnlineServiceImpl.class);

	@Autowired
	private SettingsService settingsService;

	@Override
	public String generateOrder(final OnlineRequest onlineRequest) throws RazorpayException, NotFoundException, ValidationException {
		LOGGER.info("Inside generate order for razor pay request {}", onlineRequest);
		String clientId = settingsService.getSettingsDetailsByNameForEncryptedFields(Constant.PAYMENT_GATEWAY_USER_NAME).getFieldValue();
		String clientSecret = settingsService.getSettingsDetailsByNameForEncryptedFields(Constant.PAYMENT_GATEWAY_SECRET).getFieldValue();
		RazorpayClient razorpayClient = new RazorpayClient(clientId, clientSecret);

		Map<String, String> headers = new HashMap<>();
		razorpayClient.addHeaders(headers);
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("amount", onlineRequest.getAmount());
		jsonObject.put("currency", onlineRequest.getCurrencyCode());
		Order order = razorpayClient.Orders.create(jsonObject);
		return order.toJson().getString("id");
	}
}
