package com.nice.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.PaymentMode;
import com.nice.dto.CashCollectionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CashCollectionMapper;
import com.nice.model.CashCollection;
import com.nice.model.Orders;
import com.nice.model.Task;
import com.nice.repository.CashCollectionRepository;
import com.nice.service.CashcollectionService;
import com.nice.service.TaskService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class CashCollectionServiceImpl implements CashcollectionService {

	@Autowired
	private CashCollectionRepository cashCollectionRepository;

	@Autowired
	private CashCollectionMapper cashCollectionMapper;

	@Autowired
	private TaskService taskService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public CashCollectionDTO addCashCollection(final CashCollectionDTO cashCollectionDTO) throws NotFoundException, ValidationException {
		CashCollection cashCollection = cashCollectionMapper.toEntity(cashCollectionDTO);
		Task task = taskService.getTaskDetail(cashCollectionDTO.getTaskId());
		if (task.getDeliveryBoy() == null || !task.getDeliveryBoy().getId().equals(cashCollectionDTO.getDeliveryboyId())) {
			throw new ValidationException(messageByLocaleService.getMessage("can.not.collect.cash.order", null));
		}
		if (cashCollectionRepository.findByTask(task).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("cash.already.collect", null));
		}
		cashCollection.setDeliveryBoy(task.getDeliveryBoy());
		Orders order = task.getOrder();
		if (PaymentMode.ONLINE.name().equals(order.getPaymentMode())) {
			throw new ValidationException(messageByLocaleService.getMessage("cash.collect.online.order", null));
		} else if (!order.getTotalOrderAmount().equals(cashCollection.getAmount())) {
			throw new ValidationException(messageByLocaleService.getMessage("collect.amount.mismatch", new Object[] { order.getTotalOrderAmount() }));
		}
		cashCollection.setOrder(order);
		cashCollection.setTask(task);
		cashCollection.setActive(true);
		return cashCollectionMapper.toDto(cashCollectionRepository.save(cashCollection));
	}

	@Override
	public CashCollectionDTO getCashCollection(final Long cashCollectionId) throws NotFoundException {
		return cashCollectionMapper.toDto(getCashCollectionDetail(cashCollectionId));
	}

	@Override
	public CashCollection getCashCollectionDetail(final Long cashCollectionId) throws NotFoundException {
		return cashCollectionRepository.findById(cashCollectionId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("cash.collection.not.found", new Object[] { cashCollectionId })));
	}

	@Override
	public Optional<CashCollection> getCashCollectionDetailForTask(final Long taskId) throws NotFoundException {
		Task task = taskService.getTaskDetail(taskId);
		return cashCollectionRepository.findByTask(task);
	}

	@Override
	public Long getCountBasedOnParams(final Long deliveryBoyId, final Date createdAt) {
		return cashCollectionRepository.getCountBasedOnParams(deliveryBoyId, createdAt);
	}

	@Override
	public List<CashCollection> getListBasedOnParams(final Integer startIndex, final Integer pageSize, final Long deliveryBoyId, final Date createdAt) {
		return cashCollectionRepository.getListBasedOnParams(startIndex, pageSize, deliveryBoyId, createdAt);
	}

}
