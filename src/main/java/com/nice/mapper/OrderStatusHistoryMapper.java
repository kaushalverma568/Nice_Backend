/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.OrderStatusDto;
import com.nice.model.OrderStatusHistory;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Jul-2020
 */
@Component
public class OrderStatusHistoryMapper {

	public OrderStatusDto toDto(final OrderStatusHistory orderStatus) {
		OrderStatusDto orderStatusDto = new OrderStatusDto();
		BeanUtils.copyProperties(orderStatus, orderStatusDto);
		return orderStatusDto;
	}

	public OrderStatusHistory toEntity(final OrderStatusDto orderStatusDTO, final Long userId) {
		OrderStatusHistory orderStatus = new OrderStatusHistory();
		BeanUtils.copyProperties(orderStatusDTO, orderStatus);
		if (orderStatusDTO.getId() == null) {
			orderStatus.setCreatedBy(userId);
		}
		orderStatus.setUpdatedBy(userId);
		return orderStatus;
	}

	public List<OrderStatusDto> toDtos(final List<OrderStatusHistory> orderStatusList) {
		List<OrderStatusDto> results = new ArrayList<>();
		for (OrderStatusHistory orderStatus : orderStatusList) {
			results.add(toDto(orderStatus));
		}
		return results;
	}
}
