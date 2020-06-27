package com.nice.jms.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ErrorHandler;

public class DefaultErrorHandler implements ErrorHandler {

	private static Logger log = LoggerFactory.getLogger(DefaultErrorHandler.class);

	@Override
	public void handleError(final Throwable t) {
		log.warn("spring jms custom error handling ...");
		log.error(t.getCause().getMessage());
	}

}
