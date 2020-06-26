package com.nice.exception;

import org.springframework.http.HttpStatus;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class ParseException extends BaseException {

	private static final long serialVersionUID = -7035356745256627318L;
	private static final HttpStatus status = HttpStatus.BAD_REQUEST;

	/**
	 *
	 */
	public ParseException() {
	}

	/**
	 * @param status
	 * @param message
	 * @param cause
	 */
	public ParseException(final String message, final Throwable cause) {
		super(status, message, cause);
	}

	/**
	 * @param status
	 * @param message
	 */
	public ParseException(final String message) {
		super(status, message);
	}

	/**
	 * @param status
	 * @param cause
	 */
	public ParseException(final Throwable cause) {
		super(status, cause);
	}

}
