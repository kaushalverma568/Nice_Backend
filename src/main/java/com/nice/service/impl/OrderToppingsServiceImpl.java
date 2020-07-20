package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

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

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("orderToppingsService")
public class OrderToppingsServiceImpl implements OrderToppingsService {

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
	 * @param  tempOrderAddon
	 * @return
	 */
	private OrderToppingsDto convertEntityToDto(final OrdersToppings orderToppings) {
		OrderToppingsDto orderToppingsDto = new OrderToppingsDto();
		BeanUtils.copyProperties(orderToppings, orderToppingsDto);
		orderToppingsDto.setOrderItemId(orderToppings.getOrderItem().getId());
		orderToppingsDto.setProductToppingsId(orderToppings.getProductToppings().getId());
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
