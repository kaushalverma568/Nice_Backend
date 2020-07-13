package com.nice.service;

import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OnlineRequest;
import com.razorpay.RazorpayException;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
public interface OnlineService {
	/**
	 * @param onlineRequest
	 * @return
	 * @throws RazorpayException
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	String generateOrder(final OnlineRequest onlineRequest) throws RazorpayException, NotFoundException, ValidationException;
}