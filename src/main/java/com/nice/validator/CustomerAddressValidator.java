package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.constant.Constant;
import com.nice.dto.CustomerAddressDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CustomerAddressService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Component
public class CustomerAddressValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private CustomerAddressService customersAddressService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return CustomerAddressDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final CustomerAddressDTO customersAddressDTO = (CustomerAddressDTO) target;

		/**
		 * Check address duplication based on streetno, landmark and building name
		 */

		try {
			if (customersAddressDTO != null && customersAddressService.isExists(customersAddressDTO)) {
				errors.rejectValue("firstName", "409", messageByLocaleService.getMessage("address.exists", new Object[] { Constant.CUSTOMER_ADDRESS }));
			}
		} catch (NotFoundException e) {
			errors.rejectValue("customerId", "409",
					messageByLocaleService.getMessage("customer.not.found", new Object[] { customersAddressDTO.getCustomerId() }));
		}
	}
}