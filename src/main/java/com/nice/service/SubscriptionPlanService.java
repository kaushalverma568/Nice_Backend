package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.SubscriptionPlanDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.SubscriptionPlan;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface SubscriptionPlanService {

	/**
	 * @param  subscriptionPlanDto
	 * @return
	 */
	boolean isExists(SubscriptionPlanDTO subscriptionPlanDto);

	/**
	 * @param  subscriptionPlanDto
	 * @return
	 * @throws NotFoundException
	 */
	SubscriptionPlanDTO addSubscriptionPlan(SubscriptionPlanDTO subscriptionPlanDto) throws NotFoundException;

	/**
	 * @param  subscriptionPlanDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	SubscriptionPlanDTO updateSubscriptionPlan(SubscriptionPlanDTO subscriptionPlanDTO) throws NotFoundException, ValidationException;

	/**
	 * @param  subscriptionPlanId
	 * @return
	 * @throws NotFoundException
	 */
	SubscriptionPlanDTO getSubscriptionPlan(Long subscriptionPlanId) throws NotFoundException;

	/**
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @return
	 */
	Page<SubscriptionPlan> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord);

	/**
	 * @param  subscriptionPlanId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long subscriptionPlanId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param  subscriptionPlanId
	 * @return
	 * @throws NotFoundException
	 */
	SubscriptionPlan getSubscriptionPlanDetail(Long subscriptionPlanId) throws NotFoundException;

}
