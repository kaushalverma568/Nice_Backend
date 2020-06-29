package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.model.SubscriptionPlan;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

public interface SubscriptionPlanService {

	/**
	 * 
	 * @param subscriptionPlanDto
	 * @return
	 */
	public boolean isExists(SubscriptionPlanDTO subscriptionPlanDto) ;

	/**
	 * 
	 * @param subscriptionPlanDto
	 * @return
	 * @throws NotFoundException
	 */
	public SubscriptionPlanDTO addSubscriptionPlan( SubscriptionPlanDTO subscriptionPlanDto) throws NotFoundException ;

	/**
	 * 
	 * @param subscriptionPlanDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	public SubscriptionPlanDTO updateSubscriptionPlan( SubscriptionPlanDTO subscriptionPlanDTO) throws NotFoundException, ValidationException;

	/**
	 * 
	 * @param subscriptionPlanId
	 * @return
	 * @throws NotFoundException
	 */
	public SubscriptionPlanDTO getSubscriptionPlan(Long subscriptionPlanId) throws NotFoundException;

	/**
	 * 
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyWord
	 * @return
	 */
	public Page<SubscriptionPlan> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord);

	/**
	 * 
	 * @param subscriptionPlanId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void changeStatus(Long subscriptionPlanId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param subscriptionPlanId
	 * @return
	 * @throws NotFoundException
	 */
	SubscriptionPlan getSubscriptionPlanDetail(Long subscriptionPlanId) throws NotFoundException;
	
}
