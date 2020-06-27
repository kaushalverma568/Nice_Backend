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
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.component.SendEmailNotificationComponent;

import net.sf.jasperreports.engine.JRException;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 19-Feb-2020
 */
@Component
public class JMSDequeuer {

	@Autowired
	private SendEmailNotificationComponent sendEmailNotificationComponent;

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
	public void receiveAllEmails(@Payload final Notification notification)
			throws IOException, GeneralSecurityException, MessagingException, NotFoundException/* , JRException */, ValidationException {
		sendEmailNotificationComponent.sendEmaillNotification(notification);
	}

	/**
	 * @param  notification
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
			throws IOException, GeneralSecurityException, MessagingException, NotFoundException/* , JRException */, ValidationException {
		sendEmailNotificationComponent.sendEmaillNotification(notification);
	}

}
