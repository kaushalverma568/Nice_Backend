/**
 *
 */
package com.nice.service;

import com.nice.dto.CheckOutDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Sep-2020
 */
public interface CheckOutService {

	/**
	 * @param deliveryType
	 * @param useWallet
	 * @param shippingAddressId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	CheckOutDTO getCheckOutPageDetails(String deliveryType, Boolean useWallet, Long shippingAddressId) throws NotFoundException, ValidationException;

}
