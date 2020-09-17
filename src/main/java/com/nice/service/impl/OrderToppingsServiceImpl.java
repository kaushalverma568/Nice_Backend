package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderToppingsDto;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrdersItem;
import com.nice.model.OrdersToppings;
import com.nice.repository.OrderToppingsRepository;
import com.nice.service.OrderItemService;
import com.nice.service.OrderToppingsService;
import com.nice.service.ProductToppingService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("orderToppingsService")
public class OrderToppingsServiceImpl implements OrderToppingsService {

	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderToppingsRepository orderToppingsRepository;

	@Autowired
	private ProductToppingService productToppingService;

	@Override
	public List<OrderToppingsDto> getOrderToppingsListForOrderItem(final Long orderItemId) throws NotFoundException, ValidationException {
		OrdersItem tempOrderItem = orderItemService.getOrderItemDetails(orderItemId);
		List<OrdersToppings> tempOrderAddonsList = orderToppingsRepository.findAllByOrderItem(tempOrderItem);
		return convertEntityToDtos(tempOrderAddonsList);
	}

	/**
	 * @param tempOrderAddon
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private OrderToppingsDto convertEntityToDto(final OrdersToppings orderToppings) throws NotFoundException, ValidationException {
		OrderToppingsDto orderToppingsDto = new OrderToppingsDto();
		BeanUtils.copyProperties(orderToppings, orderToppingsDto);
		orderToppingsDto.setOrderItemId(orderToppings.getOrderItem().getId());
		orderToppingsDto.setProductToppingsId(orderToppings.getProductToppings().getId());
		ProductToppingResponseDTO productToppingResponseDto = productToppingService.getProductToppingWithOutUserCheck(orderToppings.getProductToppings().getId());
		orderToppingsDto.setToppingsName(productToppingResponseDto.getName());
		return orderToppingsDto;
	}

	/**
	 * @param orderAddonsList
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private List<OrderToppingsDto> convertEntityToDtos(final List<OrdersToppings> orderAddonsList) throws NotFoundException, ValidationException {
		List<OrderToppingsDto> orderToppingsDtoList = new ArrayList<>();
		for (OrdersToppings orderTopping : orderAddonsList) {
			orderToppingsDtoList.add(convertEntityToDto(orderTopping));
		}
		return orderToppingsDtoList;
	}
}
