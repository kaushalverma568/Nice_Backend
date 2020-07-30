package com.nice.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.VendorStatus;
import com.nice.dto.SubscriptionPlanDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SubscriptionPlanMapper;
import com.nice.model.SubscriptionPlan;
import com.nice.repository.SubscriptionPlanRepository;
import com.nice.service.SubscriptionPlanService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class SubscriptionPlanServiceImpl implements SubscriptionPlanService {

	private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionPlanServiceImpl.class);

	@Autowired
	private SubscriptionPlanRepository subscriptionPlanRepository;

	@Autowired
	private SubscriptionPlanMapper subscriptionPlanMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private VendorService vendorService;

	@Override
	public SubscriptionPlanDTO addSubscriptionPlan(final SubscriptionPlanDTO subscriptionPlanDTO) throws NotFoundException {
		return subscriptionPlanMapper.toDto(subscriptionPlanRepository.save(subscriptionPlanMapper.toEntity(subscriptionPlanDTO)));
	}

	@Override
	public SubscriptionPlanDTO updateSubscriptionPlan(final SubscriptionPlanDTO subscriptionPlanDTO) throws NotFoundException, ValidationException {
		if (subscriptionPlanDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("subscription.plan.id.not.null", null));
		}
		return subscriptionPlanMapper.toDto(subscriptionPlanRepository.save(subscriptionPlanMapper.toEntity(subscriptionPlanDTO)));
	}

	@Override
	public SubscriptionPlanDTO getSubscriptionPlan(final Long subscriptionPlanId) throws NotFoundException {
		return subscriptionPlanMapper.toDto(getSubscriptionPlanDetail(subscriptionPlanId));
	}

	@Override
	public void changeStatus(final Long subscriptionPlanId, final Boolean active) throws ValidationException, NotFoundException {
		SubscriptionPlan existingSubscriptionPlan = getSubscriptionPlanDetail(subscriptionPlanId);
		LOGGER.info("Existing  SubscriptionPlan details {} ", existingSubscriptionPlan);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingSubscriptionPlan.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "subscription.plan.active" : "subscription.plan.deactive", null));
		} else {
			/**
			 * if active vendor contains this plan then can't de-active
			 */
			if (Boolean.FALSE.equals(active)) {
				VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
				vendorFilterDTO.setActiveRecords(true);
				vendorFilterDTO.setSubscriptionPlanId(subscriptionPlanId);
				vendorFilterDTO.setStatus(VendorStatus.ACTIVE.getStatusValue());
				Long vendorCount = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
				if (vendorCount > 0) {
					throw new ValidationException(messageByLocaleService.getMessage("subscription.plan.not.update.vendor", null));
				}
			}
			existingSubscriptionPlan.setActive(active);
			subscriptionPlanRepository.save(existingSubscriptionPlan);
		}
	}

	@Override
	public Page<SubscriptionPlan> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyWord) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (activeRecords != null) {
			if (searchKeyWord != null) {
				return subscriptionPlanRepository.findAllByActiveAndNameContainingIgnoreCase(activeRecords, searchKeyWord, pageable);
			} else {
				return subscriptionPlanRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyWord != null) {
				return subscriptionPlanRepository.findAllByNameContainingIgnoreCase(searchKeyWord, pageable);
			} else {
				return subscriptionPlanRepository.findAll(pageable);
			}
		}
	}

	@Override
	public boolean isExists(final SubscriptionPlanDTO subscriptionPlanDTO) {
		if (subscriptionPlanDTO.getId() != null) {
			return !(subscriptionPlanRepository.findByNameIgnoreCaseAndIdNot(subscriptionPlanDTO.getName(), subscriptionPlanDTO.getId()).isEmpty());

		} else {
			return !(subscriptionPlanRepository.findByNameIgnoreCase(subscriptionPlanDTO.getName()).isEmpty());
		}
	}

	@Override
	public SubscriptionPlan getSubscriptionPlanDetail(final Long SubscriptionPlanId) throws NotFoundException {
		return subscriptionPlanRepository.findById(SubscriptionPlanId).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("subscription.plan.not.found", new Object[] { SubscriptionPlanId })));
	}

}
