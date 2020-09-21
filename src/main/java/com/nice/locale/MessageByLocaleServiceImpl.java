package com.nice.locale;

import java.util.Locale;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class MessageByLocaleServiceImpl implements MessageByLocaleService {

	@Autowired
	private MessageSource messageSource;

	/**
	 * If no locale specified take English as local and display messages.
	 */
	@Override
	public String getMessage(final String id, final Object[] arg) {
		final Locale locale = LocaleContextHolder.getLocale();
		return messageSource.getMessage(id, arg, locale);
	}
}