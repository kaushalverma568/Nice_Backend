package com.nice.jms.queue;

import com.nice.service.impl.DeliveryBoyServiceImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.nice.constant.Constant;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.SettingsConstant;
import com.nice.dto.Notification;
import com.nice.dto.PushNotificationDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service
public class JMSQueuerService {

	private static final Logger LOGGER = LoggerFactory.getLogger(JMSQueuerService.class);


	@Autowired
	private JMSQueuer jmsQueuer;

	public void sendEmail(final String queueName, final Notification emailNotification) {
		/**
		 * if sendEmail in settings is true then only send the email
		 */
		LOGGER.info("Inside sendEmail of JMS"+ emailNotification.getEmail());
		if (((emailNotification != null) && "true".equals(SettingsConstant.getSettingsValue("SEND_EMAIL")))
				|| NotificationQueueConstants.NON_NOTIFICATION_QUEUE.equals(queueName)) {
			if ((emailNotification != null) && emailNotification.getLanguage() == null) {
				emailNotification.setLanguage(Constant.DEFAULT_LANGUAGE);
			}
			jmsQueuer.sendEmail(queueName, emailNotification);
		}
	}

	public void sendPushNotification(final String queueName, final PushNotificationDTO pushNotification) {
		/**
		 * if send push notification only when dto is not null
		 */
		if ((pushNotification != null)) {
			if (pushNotification.getLanguage() == null) {
				pushNotification.setLanguage(Constant.DEFAULT_LANGUAGE);
			}
			jmsQueuer.sendPushNotification(queueName, pushNotification);
		}
	}
}