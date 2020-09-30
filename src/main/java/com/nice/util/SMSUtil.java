package com.nice.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
@Component
public final class SMSUtil {

	private static final Logger LOGGER = LoggerFactory.getLogger(SMSUtil.class);

	private static final String smsUrl = "https://www.smsbox.com/SMSGateway/Services/Messaging.asmx/Http_SendSMS";
	private static final String userName = "Nice";
	private static final String password = "Fa9875477";
	private static final String senderText = "SMSBOX.COM";
	private static final Long customerId = 2409L;

	public boolean sendSMS(final String mobileNo, final String msg) throws ValidationException {
		boolean flag = false;
//		UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(smsUrl);
//		builder.queryParam("username", userName);
//		builder.queryParam("password", password);
//		builder.queryParam("customerId", customerId);
//		builder.queryParam("senderText", senderText);
//		builder.queryParam("messageBody", msg);
//		builder.queryParam("recipientNumbers", mobileNo);
//		builder.queryParam("isBlink", false);
//		builder.queryParam("isFlash", false);
//		builder.queryParam("defDate", "");
//		HttpHeaders headers = new HttpHeaders();
//		headers.setContentType(MediaType.APPLICATION_XML);

		/**
		 * currently we have only added code but send sms will not work
		 */
//		ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

//		RestTemplate restTemplate = new RestTemplate(requestFactory);
//		ResponseEntity<String> result = restTemplate.getForEntity(builder.build().toUri(), String.class);
//		XmlMapper xmlMapper = new XmlMapper();
//		try {
//			String xmlString = result.getBody().replace("<?xml version=\"1.0\" encoding=\"utf-8\"?>", "");
//			SendingSMSResult sendingSMSResult = xmlMapper.readValue(xmlString, SendingSMSResult.class);
//			if (sendingSMSResult.getResult().booleanValue()) {
//				if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(sendingSMSResult.getRejectedNumbers())) {
//					LOGGER.info("Rejected numbers are : {}", sendingSMSResult.getRejectedNumbers().stream().collect(Collectors.joining(",")));
//					flag = false;
//				}
//				LOGGER.info("SMS sent successfully.");
//				flag = true;
//			} else {
//				LOGGER.info("Error in sending sms : {}", sendingSMSResult.getMessage());
//				flag = false;
//			}
//		} catch (JsonProcessingException e) {
//			e.printStackTrace();
//			LOGGER.info("JSON processing exception in Send SMS : {}", e.getMessage());
//			flag = false;
//		}
		return flag;
	}

}