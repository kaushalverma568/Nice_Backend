package com.nice.service;

import java.io.IOException;

import javax.mail.MessagingException;

import com.nice.dto.GmailCredentials;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface GmailService {

	void setGmailCredentials(GmailCredentials gmailCredentials);

	boolean sendMessage(String recipientAddress, String subject, String body) throws MessagingException, IOException;
}
