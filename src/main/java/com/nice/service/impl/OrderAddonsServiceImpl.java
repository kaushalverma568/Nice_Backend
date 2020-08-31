package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderAddonsDTO;
import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrdersAddons;
import com.nice.model.OrdersItem;
import com.nice.model.ProductAddons;
import com.nice.repository.OrderAddonsRepository;
import com.nice.service.OrderAddonsService;
import com.nice.service.OrderItemService;
import com.nice.service.ProductAddonsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("orderAddonsService")
public class OrderAddonsServiceImpl implements OrderAddonsService {

	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private OrderAddonsRepository orderAddonsRepository;

	@Autowired
	private ProductAddonsService productAddonsService;

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
	 * @param orderAddon
	 * @return
	 * @throws NotFoundException
	 */
	private OrderAddonsDTO convertEntityToDto(final OrdersAddons orderAddon) throws NotFoundException {

		OrderAddonsDTO orderAddonsDto = new OrderAddonsDTO();
		BeanUtils.copyProperties(orderAddon, orderAddonsDto);
		orderAddonsDto.setOrderItemtId(orderAddon.getOrderItem().getId());
		if (orderAddon.getProductAddons() != null) {
			orderAddonsDto.setProductAddonsId(orderAddon.getProductAddons().getId());
			ProductAddonsDTO productAddonsDto = productAddonsService.getProductAddons(orderAddon.getProductAddons().getId());
			orderAddonsDto.setAddonsName(productAddonsDto.getAddonsName());
		}
		return orderAddonsDto;
	}

	/**
	 * @param orderAddonsList
	 * @throws NotFoundException
	 */
	private List<OrderAddonsDTO> convertEntityToDtos(final List<OrdersAddons> orderAddonsList) throws NotFoundException {
		List<OrderAddonsDTO> orderAddonsDtoList = new ArrayList<>();
		for (OrdersAddons orderAddons : orderAddonsList) {
			orderAddonsDtoList.add(convertEntityToDto(orderAddons));
		}
		return orderAddonsDtoList;
	}

}
