package com.nice.exception;

import org.springframework.http.HttpStatus;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class BaseException extends Exception {

	/**
	 *
	 */
	private static final long serialVersionUID = -8453363075793355923L;
	/**
	 *
	 */
	private final HttpStatus status;

	/**
	 *
	 */
	public BaseException() {
		status = HttpStatus.INTERNAL_SERVER_ERROR;
	}

	/**
	 * @param message
	 * @param cause
	 */
	public BaseException(final HttpStatus status, final String message, final Throwable cause) {
		super(message, cause);
		this.status = status;
	}

	/**
	 * @param message
	 */
	public BaseException(final HttpStatus status, final String message) {
		super(message);
		this.status = status;
	}

	/**
	 * @param cause
	 */
	public BaseException(final HttpStatus status, final Throwable cause) {
		super(cause);
		this.status = status;
	}

	public HttpStatus getStatus() {
		return status;
	}
}