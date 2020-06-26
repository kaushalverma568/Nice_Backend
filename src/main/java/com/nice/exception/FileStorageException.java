package com.nice.exception;

import org.springframework.http.HttpStatus;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class FileStorageException extends BaseRuntimeException {

	/**
	 *
	 */
	private static final long serialVersionUID = 4821105608482639763L;

	private static final HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;

	/**
	 *
	 */
	public FileStorageException() {
	}

	/**
	 * @param status
	 * @param message
	 * @param cause
	 */
	public FileStorageException(final String message, final Throwable cause) {
		super(status, message, cause);
	}

	/**
	 * @param status
	 * @param message
	 */
	public FileStorageException(final String message) {
		super(status, message);
	}

	/**
	 * @param status
	 * @param cause
	 */
	public FileStorageException(final Throwable cause) {
		super(status, cause);
	}

}
