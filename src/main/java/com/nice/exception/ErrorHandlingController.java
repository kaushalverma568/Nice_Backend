package com.nice.exception;

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.MissingRequestHeaderException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@ControllerAdvice(basePackages = "com.nice")
public class ErrorHandlingController {

	private final Logger logger = LoggerFactory.getLogger(this.getClass());

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Central exception handler and generate common custom response
	 *
	 * @param  request
	 * @param  exception
	 * @return
	 */
	@ExceptionHandler(Throwable.class)
	@ResponseBody
	ResponseEntity<Object> handleControllerException(final HttpServletRequest request, final Throwable exception, final Locale locale) {
		HttpStatus status = null;
		String message = null;
		if (exception instanceof BaseException) {
			status = ((BaseException) exception).getStatus();
			message = exception.getMessage();
		} else if (exception instanceof BaseRuntimeException) {
			status = (((BaseRuntimeException) exception).getStatus());
			message = exception.getMessage();
		} else if (exception instanceof WebApplicationException) {
			status = HttpStatus.valueOf(((WebApplicationException) exception).getResponse().getStatus());
			message = exception.getMessage();
		} else if (exception instanceof AccessDeniedException) {
			status = HttpStatus.UNAUTHORIZED;
			message = exception.getMessage();
		} else if (exception instanceof MissingServletRequestParameterException || exception instanceof MissingRequestHeaderException
				|| exception instanceof HttpMessageNotReadableException) {
			status = HttpStatus.INTERNAL_SERVER_ERROR;
			message = exception.getMessage();
		} else if (exception instanceof MethodArgumentTypeMismatchException) {
			status = HttpStatus.INTERNAL_SERVER_ERROR;
			message = "Argument mis matched";
		} else {
			status = HttpStatus.INTERNAL_SERVER_ERROR;
			message = messageByLocaleService.getMessage("common.error", null);
			StringBuffer requestedURL = request.getRequestURL();
			logger.info("Requested URL:{}", requestedURL);
			logger.error("exception : {}", exception);
		}

		return new GenericResponseHandlers.Builder().setStatus(status).setMessage(message).create();
	}

}
