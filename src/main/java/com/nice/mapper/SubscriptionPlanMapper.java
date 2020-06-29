package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.model.SubscriptionPlan;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public class SubscriptionPlanMapper {
	
	public SubscriptionPlanDTO toDto(final SubscriptionPlan subscriptionPlan) {
		SubscriptionPlanDTO subscriptionPlanResponseDTO = new SubscriptionPlanDTO();
		BeanUtils.copyProperties(subscriptionPlan, subscriptionPlanResponseDTO);
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
