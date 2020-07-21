package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.OrdersItem;
import com.nice.model.OrdersProductAttributeValue;
import com.nice.repository.OrderProductAttributeValueRepository;
import com.nice.service.OrderItemService;
import com.nice.service.OrderProductAttributeValueService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("orderAttributeValueService")
public class OrderProductAttributeValueServiceImpl implements OrderProductAttributeValueService {

	@Autowired
	private OrderItemService tempOrderItemService;

	@Autowired
	private OrderProductAttributeValueRepository orderAttributeValueRepository;

	@Override
	public List<OrderProductAttributeValueDTO> getOrderProductAttributeValueListForOrderItem(final Long orderItemId) throws NotFoundException {

		OrdersItem tempOrderItem = tempOrderItemService.getOrderItemDetails(orderItemId);
		List<OrdersProductAttributeValue> tempOrderAttributeValueList = orderAttributeValueRepository.findAllByOrderItem(tempOrderItem);
		return convertEntityToDtos(tempOrderAttributeValueList);
	}

	/**
	 * @param tempOrderAttributeValueList
	 */
	private List<OrderProductAttributeValueDTO> convertEntityToDtos(final List<OrdersProductAttributeValue> tempOrderAttributeValueList) {
		List<OrderProductAttributeValueDTO> orderAttributeValueDtoList = new ArrayList<>();
		for (OrdersProductAttributeValue tempOrderAttributeValue : tempOrderAttributeValueList) {
			OrderProductAttributeValueDTO orderProductAttributeValueDTO = new OrderProductAttributeValueDTO();
			BeanUtils.copyProperties(tempOrderAttributeValue, orderProductAttributeValueDTO);
			orderProductAttributeValueDTO.setProductAttributeValueId(tempOrderAttributeValue.getProductAttributeValue().getId());
			orderProductAttributeValueDTO.setOrderItemId(tempOrderAttributeValue.getOrderItem().getId());
			orderAttributeValueDtoList.add(orderProductAttributeValueDTO);
		}
		return orderAttributeValueDtoList;
	}

}
