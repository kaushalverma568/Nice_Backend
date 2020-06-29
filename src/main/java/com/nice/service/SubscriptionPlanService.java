package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.model.SubscriptionPlan;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

public interface SubscriptionPlanService {

	public boolean isExists(SubscriptionPlanDTO subscriptionPlanDto) ;

	public SubscriptionPlanDTO addSubscriptionPlan( SubscriptionPlanDTO subscriptionPlanDto) throws NotFoundException ;

	public SubscriptionPlanDTO updateSubscriptionPlan( SubscriptionPlanDTO subscriptionPlanDTO) throws NotFoundException, ValidationException;

	public SubscriptionPlanDTO getSubscriptionPlan(Long subscriptionPlanId) throws NotFoundException;

	public Page<SubscriptionPlan> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord);

	public void changeStatus(Long subscriptionPlanId, Boolean active) throws ValidationException, NotFoundException;

	SubscriptionPlan getSubscriptionPlanDetail(Long subscriptionPlanId) throws NotFoundException;
	
}
