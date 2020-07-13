package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderToppingsDto;
import com.nice.exception.NotFoundException;
import com.nice.model.OrdersItem;
import com.nice.model.OrdersToppings;
import com.nice.repository.OrderToppingsRepository;
import com.nice.service.OrderItemService;
import com.nice.service.OrderToppingsService;

@Transactional(rollbackFor = Throwable.class)
@Service("orderToppingsService")
public class OrderToppingsServiceImpl implements OrderToppingsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(OrderToppingsServiceImpl.class);
	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderToppingsRepository orderToppingsRepository;

	@Override
	public List<OrderToppingsDto> getOrderToppingsListForOrderItem(final Long orderItemId) throws NotFoundException {
		OrdersItem tempOrderItem = orderItemService.getOrderItemDetails(orderItemId);
		List<OrdersToppings> tempOrderAddonsList = orderToppingsRepository.findAllByOrderItem(tempOrderItem);
		return convertEntityToDtos(tempOrderAddonsList);
	}

	/**
	 *
	 * @param tempOrderAddon
	 * @return
	 */
	private OrderToppingsDto convertEntityToDto(final OrdersToppings tempOrderAddon) {
		OrderToppingsDto orderToppingsDto = new OrderToppingsDto();
		BeanUtils.copyProperties(tempOrderAddon, orderToppingsDto);
		return orderToppingsDto;
	}

	/**
	 * @param orderAddonsList
	 */
	private List<OrderToppingsDto> convertEntityToDtos(final List<OrdersToppings> orderAddonsList) {
		List<OrderToppingsDto> orderToppingsDtoList = new ArrayList<>();
		for (OrdersToppings orderTopping : orderAddonsList) {
			orderToppingsDtoList.add(convertEntityToDto(orderTopping));
		}
		return orderToppingsDtoList;
	}
}
