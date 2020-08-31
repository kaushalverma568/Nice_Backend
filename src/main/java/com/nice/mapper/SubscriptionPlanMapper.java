package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.model.SubscriptionPlan;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component
public class SubscriptionPlanMapper {

	public SubscriptionPlanDTO toDto(final SubscriptionPlan subscriptionPlan) {
		final Locale locale = LocaleContextHolder.getLocale();
		SubscriptionPlanDTO subscriptionPlanResponseDTO = new SubscriptionPlanDTO();
		BeanUtils.copyProperties(subscriptionPlan, subscriptionPlanResponseDTO);
		if (locale.getLanguage().equals("en")) {
			subscriptionPlanResponseDTO.setName(subscriptionPlan.getNameEnglish());
			subscriptionPlanResponseDTO.setDescription(subscriptionPlan.getDescriptionEnglish());
		} else {
			subscriptionPlanResponseDTO.setName(subscriptionPlan.getNameArabic());
			subscriptionPlanResponseDTO.setDescription(subscriptionPlan.getDescriptionArabic());
		}
		return subscriptionPlanResponseDTO;
	}

	public SubscriptionPlan toEntity(final SubscriptionPlanDTO subscriptionPlanDTO) {
		SubscriptionPlan subscriptionPlan = new SubscriptionPlan();
		BeanUtils.copyProperties(subscriptionPlanDTO, subscriptionPlan);
		return subscriptionPlan;
	}

	public List<SubscriptionPlanDTO> toDtos(final List<SubscriptionPlan> subscriptionPlanList) {
		List<SubscriptionPlanDTO> results = new ArrayList<>();
		for (SubscriptionPlan SubscriptionPlan : subscriptionPlanList) {
			results.add(toDto(SubscriptionPlan));
		}
		return results;
	}
}
