package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

@Transactional(rollbackFor = Throwable.class)
@Service("orderAttributeValueService")
public class OrderProductAttributeValueServiceImpl implements OrderProductAttributeValueService {
	private static final Logger LOGGER = LoggerFactory.getLogger(OrderProductAttributeValueServiceImpl.class);
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
