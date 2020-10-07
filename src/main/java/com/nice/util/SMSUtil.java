package com.nice.util;

import java.io.StringReader;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.http.impl.client.HttpClients;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.nice.constant.SettingsConstant;
import com.nice.dto.SendingSMSResult;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
@Component
public final class SMSUtil {

	private static final Logger LOGGER = LoggerFactory.getLogger(SMSUtil.class);

	private static final String SMS_URL = "https://www.smsbox.com/SMSGateway/Services/Messaging.asmx/Http_SendSMS";
	private static final String USERNAME = "Nice";
	private static final String PASSWORD = "Fa9875477";
	private static final String SENDER_TEXT = "SMSBOX.COM";
	private static final Long SMS_CUSTOMER_ID = 2409L;

	public boolean sendSMS(final String mobileNo, final String msg) {
		boolean flag = false;
		if ("true".equals(SettingsConstant.getSettingsValue("SEND_SMS"))) {
			UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(SMS_URL);
			builder.queryParam("username", USERNAME);
			builder.queryParam("password", PASSWORD);
			builder.queryParam("customerId", SMS_CUSTOMER_ID);
			builder.queryParam("senderText", SENDER_TEXT);
			builder.queryParam("messageBody", msg);
			builder.queryParam("recipientNumbers", mobileNo);
			builder.queryParam("isBlink", false);
			builder.queryParam("isFlash", false);
			builder.queryParam("defDate", "");
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_XML);

			/**
			 * currently we have only added code but send sms will not work
			 */
			ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

			RestTemplate restTemplate = new RestTemplate(requestFactory);
			ResponseEntity<String> result = restTemplate.getForEntity(builder.build().toUri(), String.class);
			try {
				JAXBContext jaxbContext = JAXBContext.newInstance(SendingSMSResult.class);

				Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();

				SendingSMSResult sendingSMSResult = (SendingSMSResult) jaxbUnmarshaller.unmarshal(new StringReader(result.getBody()));
				if (sendingSMSResult.isResult()) {
					if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(sendingSMSResult.getRejectedNumbers())) {
						String rejectedNums = sendingSMSResult.getRejectedNumbers().stream().collect(Collectors.joining(","));
						LOGGER.info("Rejected numbers are : {}", rejectedNums);
					}
					LOGGER.info("SMS sent successfully.");
					flag = true;
				} else {
					LOGGER.info("Error in sending to number {} and sms : {}", mobileNo, sendingSMSResult.getMessage());
				}
			} catch (JAXBException e) {
				e.printStackTrace();
				LOGGER.info("Xml processing exception in Send SMS : {}", e.getMessage());
			}
		} else {
			LOGGER.info("Can not send SMS due to SMS flag is false");
		}
		return flag;
	}

}