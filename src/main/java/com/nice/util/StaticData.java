package com.nice.util;

import javax.validation.ValidationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import com.nice.constant.SettingsConstant;
import com.nice.service.SettingsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class StaticData implements CommandLineRunner {

	@Autowired
	private SettingsService settingsService;

	private final Logger logger = LoggerFactory.getLogger(this.getClass());

	@Override
	public void run(final String... args) throws Exception {
		try {
			SettingsConstant.setSettingsValue("SEND_SMS", settingsService.getSettingsDetailsByNameForNonEncryptedFields("SEND_SMS").getFieldValue());
			SettingsConstant.setSettingsValue("SEND_EMAIL", settingsService.getSettingsDetailsByNameForNonEncryptedFields("SEND_EMAIL").getFieldValue());
			SettingsConstant.setSettingsValue("SMS_API_KEY", settingsService.getSettingsDetailsByNameForNonEncryptedFields("SMS_API_KEY").getFieldValue());
		} catch (ValidationException e) {
			logger.error("Error in load static data method");
		}
	}
}
