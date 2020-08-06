package com.nice.validator;

import java.time.LocalDate;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.CustomerDTO;
import com.nice.dto.CustomerPersonalDetailsDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.CustomerService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class CustomerValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private CustomerService customerService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return CustomerDTO.class.equals(clazz) || CustomerPersonalDetailsDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof CustomerDTO) {
			final CustomerDTO customerDTO = (CustomerDTO) target;
			if (customerDTO.getBirthDate() != null && LocalDate.now().compareTo(customerDTO.getBirthDate().toLocalDate()) < 0) {
				errors.rejectValue("birthdate", "409", messageByLocaleService.getMessage("birthdate.less.than.today", null));
			}
			/**
			 * Check Customer
			 */

			if (customerService.isExists(customerDTO)) {
				errors.rejectValue("email", "409", messageByLocaleService.getMessage("customer.email.exists", null));
			}

			if (customerDTO.getPhoneNumber() != null && customerService.isPhoneExists(customerDTO)) {
				errors.rejectValue("phoneNumber", "409", messageByLocaleService.getMessage("customer.phone.exists", null));
			}
		}
	}
}