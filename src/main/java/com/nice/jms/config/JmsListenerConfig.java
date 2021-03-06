package com.nice.jms.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jms.annotation.EnableJms;
import org.springframework.jms.annotation.JmsListenerConfigurer;
import org.springframework.jms.config.JmsListenerEndpointRegistrar;
import org.springframework.messaging.converter.MappingJackson2MessageConverter;
import org.springframework.messaging.converter.MessageConverter;
import org.springframework.messaging.handler.annotation.support.DefaultMessageHandlerMethodFactory;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@EnableJms
@Configuration
public class JmsListenerConfig implements JmsListenerConfigurer {

	@Bean
	public DefaultMessageHandlerMethodFactory handlerMethodFactory() {
		final DefaultMessageHandlerMethodFactory factory = new DefaultMessageHandlerMethodFactory();
		factory.setMessageConverter(messageConverter());
		return factory;
	}

	@Bean
	public MessageConverter messageConverter() {
		return new MappingJackson2MessageConverter();
	}

	@Override
	public void configureJmsListeners(final JmsListenerEndpointRegistrar registrar) {
		registrar.setMessageHandlerMethodFactory(handlerMethodFactory());
	}

}
