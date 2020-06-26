package com.nice.util;

import org.springframework.stereotype.Component;

import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Component
public final class SMSUtil {

	/*
	 * by logging, display operation detail in console
	 */

	public boolean sendSMS(final String mobileNo, final String msg) throws ValidationException {
		return false;
	}

}