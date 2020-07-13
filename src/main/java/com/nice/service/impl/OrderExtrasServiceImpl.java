package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderExtrasDto;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.OrdersExtras;
import com.nice.model.OrdersItem;
import com.nice.repository.OrderExtrasRepository;
import com.nice.service.OrderExtrasService;
import com.nice.service.OrderItemService;

@Transactional(rollbackFor = Throwable.class)
@Service("orderExtrasService")
public class OrderExtrasServiceImpl implements OrderExtrasService {
	private static final Logger LOGGER = LoggerFactory.getLogger(OrderExtrasServiceImpl.class);
	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderExtrasRepository orderExtrasRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public List<OrderExtrasDto> getOrderExtrasListForOrderItem(final Long orderItemId) throws NotFoundException {
		OrdersItem orderItem = orderItemService.getOrderItemDetails(orderItemId);
		List<OrdersExtras> orderExtrasList = orderExtrasRepository.findAllByOrderItem(orderItem);
		return convertEntityToDtos(orderExtrasList);
	}

	/**
	 *
	 * @param orderExtras
	 * @return
	 */
	private OrderExtrasDto convertEntityToDto(final OrdersExtras orderExtras) {
		OrderExtrasDto orderExtrasDto = new OrderExtrasDto();
		BeanUtils.copyProperties(orderExtras, orderExtrasDto);
		return orderExtrasDto;
	}

	/**
	 * @param orderExtrasList
	 */
	private List<OrderExtrasDto> convertEntityToDtos(final List<OrdersExtras> orderExtrasList) {
		List<OrderExtrasDto> orderExtrasDtoList = new ArrayList<>();
		for (OrdersExtras orderExtras : orderExtrasList) {
			orderExtrasDtoList.add(convertEntityToDto(orderExtras));
		}
		return orderExtrasDtoList;
	}

}
