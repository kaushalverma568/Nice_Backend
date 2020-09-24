package com.nice.jms.deque;

import java.io.IOException;
import java.security.GeneralSecurityException;

import javax.mail.MessagingException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.nice.constant.NotificationQueueConstants;
import com.nice.dto.Notification;
import com.nice.dto.PushNotificationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.component.SendEmailNotificationComponent;
import com.nice.jms.component.SendPushNotificationComponent;

import net.sf.jasperreports.engine.JRException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component
public class JMSDequeuer {

	@Autowired
	private SendEmailNotificationComponent sendEmailNotificationComponent;

	@Autowired
	private SendPushNotificationComponent sendPushNotifictionComponent;

	/**
	 * receive all the emails
	 *
	 * @throws MessagingException
	 * @throws GeneralSecurityException
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws JRException
	 */
	@JmsListener(destination = NotificationQueueConstants.GENERAL_QUEUE)
	@JsonDeserialize(as = Notification.class)
	public void receiveAllEmails(@Payload final Notification notification) throws IOException, GeneralSecurityException, MessagingException, NotFoundException {
		sendEmailNotificationComponent.sendEmaillNotification(notification);
	}

	/**
	 * @param notification
	 * @throws IOException
	 * @throws GeneralSecurityException
	 * @throws MessagingException
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws JRException
	 */
	@JmsListener(destination = NotificationQueueConstants.NON_NOTIFICATION_QUEUE)
	@JsonDeserialize(as = Notification.class)
	public void receiveAllNonNotificationEmails(@Payload final Notification notification)
			throws IOException, GeneralSecurityException, MessagingException, NotFoundException {
		sendEmailNotificationComponent.sendEmaillNotification(notification);
	}

	/**
	 * receive all push notifications
	 *
	 * @param pushNotification
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws IOException
	 */
	@JmsListener(destination = NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE)
	@JsonDeserialize(as = PushNotificationDTO.class)
	public void receiveAllPushNotifications(@Payload final PushNotificationDTO pushNotification) throws NotFoundException, ValidationException, IOException {
		sendPushNotifictionComponent.addPushNotification(pushNotification);
	}
}
