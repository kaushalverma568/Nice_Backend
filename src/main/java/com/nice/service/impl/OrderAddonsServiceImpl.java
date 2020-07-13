package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.OrdersAddons;
import com.nice.model.OrdersItem;
import com.nice.model.ProductAddons;
import com.nice.repository.OrderAddonsRepository;
import com.nice.service.OrderAddonsService;
import com.nice.service.OrderItemService;
import com.nice.service.ProductAddonsService;

@Transactional(rollbackFor = Throwable.class)
@Service("orderAddonsService")
public class OrderAddonsServiceImpl implements OrderAddonsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(OrderAddonsServiceImpl.class);
	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderAddonsRepository orderAddonsRepository;

	@Autowired
	private ProductAddonsService productAddonsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addOrderAddons(final OrderAddonsDTO orderAddonsDTO, final OrdersItem orderItem) throws ValidationException, NotFoundException {
		OrdersAddons orderAddons = new OrdersAddons();
		BeanUtils.copyProperties(orderAddonsDTO, orderAddons);
		ProductAddons productAddons = productAddonsService.getProductAddonsDetail(orderAddonsDTO.getProductAddonsId());
		orderAddons.setProductAddons(productAddons);
		orderAddons.setOrderItem(orderItem);
		orderAddonsRepository.save(orderAddons);
	}

	@Override
	public List<OrderAddonsDTO> getOrderAddonsListForOrderItem(final Long orderItemId) throws NotFoundException {
		OrdersItem orderItem = orderItemService.getOrderItemDetails(orderItemId);
		List<OrdersAddons> orderAddonsList = orderAddonsRepository.findAllByOrderItem(orderItem);
		return convertEntityToDtos(orderAddonsList);
	}

	/**
	 *
	 * @param orderAddon
	 * @return
	 */
	private OrderAddonsDTO convertEntityToDto(final OrdersAddons orderAddon) {
		OrderAddonsDTO orderAddonsDto = new OrderAddonsDTO();
		BeanUtils.copyProperties(orderAddon, orderAddonsDto);
		return orderAddonsDto;
	}

	/**
	 * @param orderAddonsList
	 */
	private List<OrderAddonsDTO> convertEntityToDtos(final List<OrdersAddons> orderAddonsList) {
		List<OrderAddonsDTO> orderAddonsDtoList = new ArrayList<>();
		for (OrdersAddons orderAddons : orderAddonsList) {
			orderAddonsDtoList.add(convertEntityToDto(orderAddons));
		}
		return orderAddonsDtoList;
	}

}
