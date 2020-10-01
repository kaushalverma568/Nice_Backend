package com.nice.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.gmail.Gmail;
import com.nice.dto.EmailContentDTO;
import com.nice.service.impl.GmailServiceImpl;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Component
public class EmailUtil {
	private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();
	private static final Logger LOGGER = LoggerFactory.getLogger(EmailUtil.class);

	@Value("${application.name}")
	private String applicationName;

	/**
	 * Send email generic method, sends email to the specified recepients in sendTo (as To)(List) in sendCc(as Cc)(List),
	 * specify the subject of the email in email Subject, also specify the template with which the email is to be sent and
	 * the emailParameterMap contains the place holders to specify for the dynamic template values.
	 *
	 * @param emailSubject
	 * @param sendTo
	 * @param emailParameterMap
	 * @param sendCc
	 * @param sendBcc
	 * @param emailTemplateName
	 * @throws GeneralSecurityException
	 * @throws IOException
	 * @throws MessagingException
	 */
	public void sendEmail(final String emailSubject, final List<String> sendTo, final Map<String, String> emailParameterMap, final List<String> sendCc,
			final List<String> sendBcc, final String emailTemplateName, final Path reportLocation, final String language)
			throws GeneralSecurityException, IOException, MessagingException {
		if (sendTo == null || sendTo.isEmpty()) {
			LOGGER.error("No recipient specified to email");
			throw new MessagingException("Please specify the receipient of email");
		}

		final NetHttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();
		Gmail service = new Gmail.Builder(httpTransport, JSON_FACTORY, GmailServiceImpl.getCredentials(httpTransport)).setApplicationName(applicationName)
				.build();
		/**
		 * Create an EmailContentDTO Object
		 */
		EmailContentDTO emailContent = new EmailContentDTO();
		emailContent.setEmailBody(GmailServiceImpl.createEmailBody(emailParameterMap, emailTemplateName, language));
		emailContent.setEmailSubject(emailSubject);
		emailContent.setEmailTo(sendTo);
		emailContent.setEmailBcc(sendBcc);
		emailContent.setEmailCc(sendCc);
		Properties props = System.getProperties();
		Session session = Session.getDefaultInstance(props);
		session.setDebug(false);
		final MimeMessage message = new MimeMessage(session);
		final MimeMessageHelper helper = new MimeMessageHelper(message, "UTF-8");
		/**
		 * Set Email to
		 */
		if (emailContent.getEmailTo() != null && !emailContent.getEmailTo().isEmpty()) {
			InternetAddress[] emailTOs = new InternetAddress[emailContent.getEmailTo().size()];
			for (int i = 0; i < emailContent.getEmailTo().size(); i++) {
				emailTOs[i] = InternetAddress.parse(emailContent.getEmailTo().get(i))[0];
			}
			helper.setTo(emailTOs);
		}

		/**
		 * Send Email as Cc
		 */
		if (emailContent.getEmailCc() != null && !emailContent.getEmailCc().isEmpty()) {
			InternetAddress[] emailCcs = new InternetAddress[emailContent.getEmailCc().size()];
			for (int i = 0; i < emailContent.getEmailCc().size(); i++) {
				emailCcs[i] = InternetAddress.parse(emailContent.getEmailCc().get(i))[0];
			}
			helper.setCc(emailCcs);
		}

		/**
		 * Send Email as Bcc
		 */
		if (emailContent.getEmailBcc() != null && !emailContent.getEmailBcc().isEmpty()) {
			InternetAddress[] emailBccs = new InternetAddress[emailContent.getEmailBcc().size()];
			for (int i = 0; i < emailContent.getEmailBcc().size(); i++) {
				emailBccs[i] = InternetAddress.parse(emailContent.getEmailBcc().get(i))[0];
			}
			helper.setBcc(emailBccs);
		}

		/**
		 *
		 */
		if (reportLocation == null) {
			helper.setText(emailContent.getEmailBody(), true);
		} else {
			Multipart multipart = new MimeMultipart();
			MimeBodyPart messageBodypart = new MimeBodyPart();
			messageBodypart.attachFile(new File(reportLocation.toString()), "application/pdf", null);
			messageBodypart.setFileName(reportLocation.getFileName().toString());
			multipart.addBodyPart(messageBodypart);

			MimeBodyPart messageBodypart1 = new MimeBodyPart();
			messageBodypart1.setContent(emailContent.getEmailBody(), "text/html");
			multipart.addBodyPart(messageBodypart1);
			message.setContent(multipart, "text/plain; charset=utf-8");
			message.saveChanges();
		}

		helper.setSubject(emailContent.getEmailSubject());
		LOGGER.info("EmailContentDto : {}", emailContent);
		GmailServiceImpl.sendMessage(service, "me", message);
	}

	/**
	 * Use this method to send emails without any attachments.
	 *
	 * @param emailSubject
	 * @param sendTo
	 * @param emailParameterMap
	 * @param sendCc
	 * @param sendBcc
	 * @param emailTemplateName
	 * @throws GeneralSecurityException
	 * @throws IOException
	 * @throws MessagingException
	 */
	public void sendEmail(final String emailSubject, final List<String> sendTo, final Map<String, String> emailParameterMap, final List<String> sendCc,
			final List<String> sendBcc, final String emailTemplateName, final String language)
			throws GeneralSecurityException, IOException, MessagingException {
		sendEmail(emailSubject, sendTo, emailParameterMap, sendCc, sendBcc, emailTemplateName, null, language);
	}

	/**
	 * Use this method to send emails to a single person without attachments.
	 *
	 * @param emailSubject
	 * @param sendTo
	 * @param emailParameterMap
	 * @param sendCc
	 * @param sendBcc
	 * @param emailTemplateName
	 * @throws GeneralSecurityException
	 * @throws IOException
	 * @throws MessagingException
	 */
	public void sendEmail(final String emailSubject, final String sendTo, final Map<String, String> emailParameterMap, final List<String> sendCc,
			final List<String> sendBcc, final String emailTemplateName, final String language)
			throws GeneralSecurityException, IOException, MessagingException {
		List<String> sendToList = new ArrayList<>();
		sendToList.add(sendTo);
		sendEmail(emailSubject, sendToList, emailParameterMap, sendCc, sendBcc, emailTemplateName, language);
	}

}
