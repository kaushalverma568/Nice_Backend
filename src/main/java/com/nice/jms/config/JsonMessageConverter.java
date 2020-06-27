package com.nice.jms.config;

import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.TextMessage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.support.converter.MessageConversionException;
import org.springframework.jms.support.converter.MessageConverter;

import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonMessageConverter implements MessageConverter {

	@Autowired
	private ObjectMapper mapper;

	/**
	 * Converts message to JSON. Used mostly by
	 * {@link org.springframework.jms.core.JmsTemplate}
	 */
	@Override
	public javax.jms.Message toMessage(final Object object, final Session session) throws JMSException {
		String json;

		try {
			json = mapper.writeValueAsString(object);
		} catch (final Exception e) {
			throw new MessageConversionException("Message cannot be parsed. ", e);
		}

		final TextMessage message = session.createTextMessage();
		message.setText(json);

		return message;
	}

	/**
	 * Extracts JSON payload for further processing by JacksonMapper.
	 */
	@Override
	public Object fromMessage(final javax.jms.Message message) throws JMSException {
		return ((TextMessage) message).getText();
	}
}
