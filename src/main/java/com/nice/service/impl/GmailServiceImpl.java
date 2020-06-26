package com.nice.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.io.ClassPathTemplateLoader;
import com.github.jknack.handlebars.io.TemplateLoader;
import com.google.api.client.auth.oauth2.Credential;
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp;
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver;
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow;
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets;
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.client.util.Base64;
import com.google.api.client.util.store.FileDataStoreFactory;
import com.google.api.services.gmail.Gmail;
import com.google.api.services.gmail.GmailScopes;
import com.google.api.services.gmail.model.Message;
import com.nice.dto.GmailCredentials;
import com.nice.service.GmailService;

/**
 * This is the default Gmail Java class to send emails.
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public final class GmailServiceImpl implements GmailService {

	private static final int PORT = 9020;

	private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();

	private final HttpTransport httpTransport;
	private GmailCredentials gmailCredentials;

	@Value("${application.name}")
	private String applicationName;

	public GmailServiceImpl(final HttpTransport httpTransport) {
		this.httpTransport = httpTransport;
	}

	@Override
	public void setGmailCredentials(final GmailCredentials gmailCredentials) {
		this.gmailCredentials = gmailCredentials;
	}

	@Override
	public boolean sendMessage(final String recipientAddress, final String subject, final String body) throws MessagingException, IOException {
		Message message = createMessageWithEmail(createEmail(recipientAddress, gmailCredentials.getUserEmail(), subject, body));

		return createGmail().users().messages().send(gmailCredentials.getUserEmail(), message).execute().getLabelIds().contains("SENT");
	}

	private Gmail createGmail() {
		Credential credential = authorize();
		return new Gmail.Builder(httpTransport, JSON_FACTORY, credential).setApplicationName(applicationName).build();
	}

	private MimeMessage createEmail(final String to, final String from, final String subject, final String bodyText) throws MessagingException {
		MimeMessage email = new MimeMessage(Session.getDefaultInstance(new Properties(), null));
		email.setFrom(new InternetAddress(from));
		email.addRecipient(javax.mail.Message.RecipientType.TO, new InternetAddress(to));
		email.setSubject(subject);
		email.setText(bodyText);
		return email;
	}

	private Message createMessageWithEmail(final MimeMessage emailContent) throws MessagingException, IOException {
		ByteArrayOutputStream buffer = new ByteArrayOutputStream();
		emailContent.writeTo(buffer);

		return new Message().setRaw(Base64.encodeBase64URLSafeString(buffer.toByteArray()));
	}

	private Credential authorize() {
		return new GoogleCredential.Builder().setTransport(httpTransport).setJsonFactory(JSON_FACTORY)
				.setClientSecrets(gmailCredentials.getClientId(), gmailCredentials.getClientSecret()).build().setAccessToken(gmailCredentials.getAccessToken())
				.setRefreshToken(gmailCredentials.getRefreshToken());
	}

	private static final String CREDENTIALS_FILE_PATH = "credential.json";
	private static final String TOKENS_DIRECTORY_PATH = "tokens";
	private static final List<String> SCOPES = Collections.singletonList(GmailScopes.MAIL_GOOGLE_COM);

	public static Credential getCredentials(final NetHttpTransport httpTransport) throws IOException {
		// Load client secrets.

		InputStream in = new ClassPathResource(CREDENTIALS_FILE_PATH).getInputStream();
		GoogleClientSecrets clientSecrets = GoogleClientSecrets.load(JSON_FACTORY, new InputStreamReader(in));

		// Build flow and trigger user authorization request.
		GoogleAuthorizationCodeFlow flow = new GoogleAuthorizationCodeFlow.Builder(httpTransport, JSON_FACTORY, clientSecrets, SCOPES)
				.setDataStoreFactory(new FileDataStoreFactory(new java.io.File(TOKENS_DIRECTORY_PATH))).setAccessType("offline").build();
		LocalServerReceiver receiver = new LocalServerReceiver.Builder().setPort(PORT).build();
		return new AuthorizationCodeInstalledApp(flow, receiver).authorize("user");
	}

	public static Message createMessageWithEmailNew(final MimeMessage emailContent) throws MessagingException, IOException {
		ByteArrayOutputStream buffer = new ByteArrayOutputStream();
		emailContent.writeTo(buffer);
		byte[] bytes = buffer.toByteArray();
		String encodedEmail = Base64.encodeBase64URLSafeString(bytes);
		Message message = new Message();
		message.setRaw(encodedEmail);
		return message;
	}

	public static Message sendMessage(final Gmail service, final String userId, final MimeMessage emailContent) throws MessagingException, IOException {
		Message message = createMessageWithEmailNew(emailContent);
		message = service.users().messages().send(userId, message).execute();
		return message;
	}

	public static String createEmailBody(final Map<String, String> bodyMap, final String template) throws IOException {
		final TemplateLoader templateLoader = new ClassPathTemplateLoader("/templates", ".html");
		final Handlebars handlebars = new Handlebars(templateLoader);
		final Template emailTemplate = handlebars.compile(template.toLowerCase());
		return emailTemplate.apply(bodyMap);
	}

}