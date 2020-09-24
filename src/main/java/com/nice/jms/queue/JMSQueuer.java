package com.nice.jms.queue;

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

	@Autowired
	private JmsTemplate jmsTemplate;

	public void sendEmail(final String queueName, final Notification emailNotification) {
		jmsTemplate.convertAndSend(queueName, emailNotification);
	}

	public void sendPushNotification(final String queueName, final PushNotificationDTO pushNotification) {
		jmsTemplate.convertAndSend(queueName, pushNotification);
	}
}
