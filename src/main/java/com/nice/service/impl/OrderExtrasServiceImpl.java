package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderExtrasDto;
import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.OrdersExtras;
import com.nice.model.OrdersItem;
import com.nice.repository.OrderExtrasRepository;
import com.nice.service.OrderExtrasService;
import com.nice.service.OrderItemService;
import com.nice.service.ProductExtrasService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("orderExtrasService")
public class OrderExtrasServiceImpl implements OrderExtrasService {

	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderExtrasRepository orderExtrasRepository;

	@Autowired
	private ProductExtrasService productExtrasService;

	@Override
	public List<OrderExtrasDto> getOrderExtrasListForOrderItem(final Long orderItemId) throws NotFoundException {
		OrdersItem orderItem = orderItemService.getOrderItemDetails(orderItemId);
		List<OrdersExtras> orderExtrasList = orderExtrasRepository.findAllByOrderItem(orderItem);
		return convertEntityToDtos(orderExtrasList);
	}

	/**
	 * @param orderExtras
	 * @return
	 * @throws NotFoundException
	 */
	private OrderExtrasDto convertEntityToDto(final OrdersExtras orderExtras) throws NotFoundException {
		OrderExtrasDto orderExtrasDto = new OrderExtrasDto();
		BeanUtils.copyProperties(orderExtras, orderExtrasDto);
		orderExtrasDto.setOrderItemId(orderExtras.getOrderItem().getId());
		if (orderExtras.getProductExtras() != null) {
			orderExtrasDto.setProductExtrasId(orderExtras.getProductExtras().getId());
			ProductExtrasDTO productExtrasDto = productExtrasService.getProductExtras(orderExtras.getProductExtras().getId());
			orderExtrasDto.setExtrasName(productExtrasDto.getName());
		}
		orderExtrasDto.setProductExtrasId(orderExtras.getProductExtras().getId());
		return orderExtrasDto;
	}

	/**
	 * @param orderExtrasList
	 * @throws NotFoundException
	 */
	private List<OrderExtrasDto> convertEntityToDtos(final List<OrdersExtras> orderExtrasList) throws NotFoundException {
		List<OrderExtrasDto> orderExtrasDtoList = new ArrayList<>();
		for (OrdersExtras orderExtras : orderExtrasList) {
			orderExtrasDtoList.add(convertEntityToDto(orderExtras));
		}
		return orderExtrasDtoList;
	}

}
