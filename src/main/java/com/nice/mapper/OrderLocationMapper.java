package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.OrderLocationDTO;
import com.nice.model.OrderLocation;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
@Component
public class OrderLocationMapper {

	public OrderLocationDTO toDto(final OrderLocation orderLocation) {
		OrderLocationDTO orderLocationDTO = new OrderLocationDTO();
		BeanUtils.copyProperties(orderLocation, orderLocationDTO);
		return orderLocationDTO;
	}

	public OrderLocation toEntity(final OrderLocationDTO orderLocationDTO) {
		OrderLocation orderLocation = new OrderLocation();
		BeanUtils.copyProperties(orderLocationDTO, orderLocation);
		return orderLocation;
	}

	public List<OrderLocationDTO> toDtos(final List<OrderLocation> orderLocations) {
		List<OrderLocationDTO> results = new ArrayList<>();
		for (OrderLocation orderLocation : orderLocations) {
			results.add(toDto(orderLocation));
		}
		return results;
	}
}
