package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderItemRatingDTO;
import com.nice.dto.OrderItemRatingResponseDTO;
import com.nice.model.OrderItemRating;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderItemRatingMapper;
import com.nice.repository.OrderItemRatingRepository;
import com.nice.service.OrderItemRatingService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Dec-2019
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class OrderItemRatingServiceImpl implements OrderItemRatingService {

	private static final Logger LOGGER = LoggerFactory.getLogger(OrderItemRatingServiceImpl.class);
	
	@Autowired
	private OrderItemRatingRepository orderItemRatingRepository;

	@Autowired
	private OrderItemRatingMapper orderItemRatingMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public OrderItemRatingResponseDTO addOrderItemRating(final OrderItemRatingDTO orderItemRatingDTO) throws NotFoundException {
		return orderItemRatingMapper.toDto(orderItemRatingRepository.save(orderItemRatingMapper.toEntity(orderItemRatingDTO)));
	}


	@Override
	public OrderItemRatingResponseDTO getOrderItemRating(final Long orderItemRatingId) throws NotFoundException {
		return orderItemRatingMapper.toDto(getOrderItemRatingDetail(orderItemRatingId));
	}

	@Override
	public void changeStatus(final Long orderItemRatingId, final Boolean active) throws ValidationException, NotFoundException {
		OrderItemRating existingOrderItemRating = getOrderItemRatingDetail(orderItemRatingId);
		LOGGER.info("Existing  OrderItemRating details {} ", existingOrderItemRating);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingOrderItemRating.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "order.item.rating.active" : "order.item.rating.deactive", null));
		} else {
			existingOrderItemRating.setActive(active);
			orderItemRatingRepository.save(existingOrderItemRating);
		}
	}

	@Override
	public Page<OrderItemRating> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (activeRecords != null) {
			return orderItemRatingRepository.findAllByActive(activeRecords, pageable);
		} else {
			return orderItemRatingRepository.findAll(pageable);
		}
	}


	@Override
	public OrderItemRating getOrderItemRatingDetail(final Long OrderItemRatingId) throws NotFoundException {
		return orderItemRatingRepository.findById(OrderItemRatingId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("order.item.rating.not.found", new Object[] {  OrderItemRatingId })));
	}

	@Override
	public List<OrderItemRating> getOrderRatingByProductId (final Long productId){
		return  orderItemRatingRepository.findByProductId(productId);
	}
	
	@Override
	public List<OrderItemRating> getOrderRatingByOrderRatingId(final Long orderRatingId) {
		return  orderItemRatingRepository.findByOrderRatingId(orderRatingId);
	}
	
}
