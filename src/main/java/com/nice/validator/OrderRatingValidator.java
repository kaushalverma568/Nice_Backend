package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.OrderRatingDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.OrderRatingService;
@Component
public class OrderRatingValidator  implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private OrderRatingService orderRatingService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return OrderRatingDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final OrderRatingDTO orderRatingDTO = (OrderRatingDTO) target;

		if ( orderRatingDTO.getOrderId() != null && orderRatingService.isExists(orderRatingDTO)) {
			errors.rejectValue("orderId", "409", messageByLocaleService.getMessage("order.rating.orderId.not.unique", null));
		}
	}
}