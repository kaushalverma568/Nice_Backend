package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.SubscriptionPlanService;
@Component
public class SubscriptionPlanValidator  implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private SubscriptionPlanService subscriptionPlanService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return SubscriptionPlanDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final SubscriptionPlanDTO subscriptionPlanDTO = (SubscriptionPlanDTO) target;

		if ( subscriptionPlanService.isExists(subscriptionPlanDTO)) {
			errors.rejectValue("name", "409", messageByLocaleService.getMessage("subscription.plan.name.not.unique", null));
		}
	}
}