package com.nice.service.impl;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CashCollectionDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CashCollectionMapper;
import com.nice.model.CashCollection;
import com.nice.repository.CashCollectionRepository;
import com.nice.service.CashcollectionService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
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
	private DeliveryBoyService deliveryBoyService;
	
	@Autowired
	private OrdersService orderService;
	
	@Autowired
	private TaskService taskService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;
	
	@Override
	public CashCollectionDTO addCashCollection(final CashCollectionDTO cashCollectionDTO) throws NotFoundException {
		CashCollection cashCollection = cashCollectionMapper.toEntity(cashCollectionDTO);
		cashCollection.setDeliveryBoy(deliveryBoyService.getDeliveryBoyDetail(cashCollectionDTO.getDeliveryboyId()));
		cashCollection.setOrder(orderService.getOrderById(cashCollectionDTO.getOrderId()));
		cashCollection.setTask(taskService.getTaskDetail(cashCollectionDTO.getTaskId()));
		return cashCollectionMapper.toDto(cashCollectionRepository.save(cashCollection));
	}

	@Override
	public CashCollectionDTO getCashCollection(final Long cashCollectionId) throws NotFoundException {
	  	return cashCollectionMapper.toDto(getCashCollectionDetail(cashCollectionId));
	}

	@Override
	public CashCollection getCashCollectionDetail(Long cashCollectionId) throws NotFoundException {
		return cashCollectionRepository.findById(cashCollectionId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("cash.collection.not.found", new Object[] {  cashCollectionId })));
	}

	@Override
	public Long getCountBasedOnParams(Long deliveryBoyId, Date createdAt) {
		return cashCollectionRepository.getCountBasedOnParams(deliveryBoyId,createdAt);
	}

	@Override
	public List<CashCollection> getListBasedOnParams(Integer startIndex, Integer pageSize, Long deliveryBoyId,
			Date createdAt) {
		return cashCollectionRepository.getListBasedOnParams(startIndex, pageSize, deliveryBoyId, createdAt);
	}

}
