package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.SubscriptionPlanService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Component
public class SubscriptionPlanValidator implements Validator {

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

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(subscriptionPlanDTO.getNameEnglish())
				&& subscriptionPlanService.isExistsEnglish(subscriptionPlanDTO)) {
			errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("subscription.plan.name.english.not.unique", null));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(subscriptionPlanDTO.getNameEnglish()) && subscriptionPlanService.isExistsArabic(subscriptionPlanDTO)) {
			errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("subscription.plan.name.arabic.not.unique", null));
		}

		if (subscriptionPlanDTO.getDays() != null && subscriptionPlanService.isDaysExist(subscriptionPlanDTO)) {
			errors.rejectValue("days", "409", messageByLocaleService.getMessage("subscription.plan.days.not.unique", null));
		}
	}
}