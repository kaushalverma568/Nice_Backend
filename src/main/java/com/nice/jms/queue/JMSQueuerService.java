package com.nice.jms.queue;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.nice.constant.Constant;
import com.nice.constant.NotificationQueueConstants;
import com.nice.dto.Notification;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 19-Feb-2020
 */
@Service
public class JMSQueuerService {

	@Autowired
	private JMSQueuer jmsQueuer;

	public void sendEmail(final String queueName, final Notification emailNotification) {
		/**
		 * if sendEmail in settings is true then only send the email
		 */
		if (((emailNotification != null) && "true".equals(Constant.getSettingsValue("SEND_EMAIL")))
				|| NotificationQueueConstants.NON_NOTIFICATION_QUEUE.equals(queueName)) {
			jmsQueuer.sendEmail(queueName, emailNotification);
		}
	}
}