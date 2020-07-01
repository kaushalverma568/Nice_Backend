package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.dto.OrderItemRatingDTO;
import com.nice.dto.OrderItemRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.OrderItemRating;
import com.nice.service.ProductService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 08-Jan-2020
 */
@Component
public class OrderItemRatingMapper {
	
	@Autowired
	private ProductService productService;
	
	
	public OrderItemRatingResponseDTO toDto(final OrderItemRating orderItemRating) throws NotFoundException {
		OrderItemRatingResponseDTO orderItemRatingResponseDTO = new OrderItemRatingResponseDTO();
		BeanUtils.copyProperties(orderItemRating, orderItemRatingResponseDTO);
		orderItemRatingResponseDTO.setProductName(productService.getProductDetail(orderItemRating.getProductId()).getName());
		return orderItemRatingResponseDTO;
	}

	public OrderItemRating toEntity(final OrderItemRatingDTO orderItemRatingDTO) {
		OrderItemRating orderItemRating = new OrderItemRating();
		BeanUtils.copyProperties(orderItemRatingDTO, orderItemRating);
		return orderItemRating;
	}

	public List<OrderItemRatingResponseDTO> toDtos(final List<OrderItemRating> orderItemRatingList) throws NotFoundException {
		List<OrderItemRatingResponseDTO> results = new ArrayList<>();
		for (OrderItemRating OrderItemRating : orderItemRatingList) {
			results.add(toDto(OrderItemRating));
		}
		return results;
	}
}
