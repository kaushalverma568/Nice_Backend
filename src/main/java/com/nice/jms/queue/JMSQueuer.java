package com.nice.jms.queue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Service;

import com.nice.dto.Notification;
import com.nice.dto.PushNotificationDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service
public class JMSQueuer {

	private static final Logger LOGGER = LoggerFactory.getLogger(JMSQueuer.class);


	@Autowired
	private JmsTemplate jmsTemplate;

	public void sendEmail(final String queueName, final Notification emailNotification) {
		LOGGER.info("Inside sendEmail of JMS Queuer"+ emailNotification.getEmail());
		jmsTemplate.convertAndSend(queueName, emailNotification);
	}

	public void sendPushNotification(final String queueName, final PushNotificationDTO pushNotification) {
		jmsTemplate.convertAndSend(queueName, pushNotification);
	}
}
