package com.nice.jms.queue;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.SettingsConstant;
import com.nice.dto.Notification;
import com.nice.dto.PushNotification;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service
public class JMSQueuerService {

	@Autowired
	private JMSQueuer jmsQueuer;

	public void sendEmail(final String queueName, final Notification emailNotification) {
		/**
		 * if sendEmail in settings is true then only send the email
		 */
		if (((emailNotification != null) && "true".equals(SettingsConstant.getSettingsValue("SEND_EMAIL")))
				|| NotificationQueueConstants.NON_NOTIFICATION_QUEUE.equals(queueName)) {
			jmsQueuer.sendEmail(queueName, emailNotification);
		}
	}

	public void sendPushNotification(final String queueName, final PushNotification pushNotification) {
		/**
		 * if send push notification only when dto is not null
		 */
		if ((pushNotification != null)) {
			jmsQueuer.sendPushNotification(queueName, pushNotification);
		}
	}
}