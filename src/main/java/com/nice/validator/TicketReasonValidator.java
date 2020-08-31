package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.TicketReasonDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.TicketReasonService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 19, 2020
 */
@Component
public class TicketReasonValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private TicketReasonService ticketReasonService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return TicketReasonDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		final TicketReasonDTO ticketReasonDTO = (TicketReasonDTO) target;
		/**
		 * Check ticketReason duplication based on name
		 */
		if (ticketReasonDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(ticketReasonDTO.getReason())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(ticketReasonDTO.getType())
				&& ticketReasonService.isTicketReasonExists(ticketReasonDTO)) {
			errors.rejectValue("reason", "409", messageByLocaleService.getMessage("ticket.reason.not.unique", null));
		}

	}

}
