package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.DeliveryBoy;
import com.nice.model.OrderRating;
import com.nice.model.Vendor;
import com.nice.service.DeliveryBoyService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jan-2020
 */
@Component
public class OrderRatingMapper {

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private VendorService vendorService;


	public OrderRatingResponseDTO toResponseDto(final OrderRating orderRating) throws NotFoundException {
		OrderRatingResponseDTO orderRatingResponseDTO = new OrderRatingResponseDTO();
		BeanUtils.copyProperties(orderRating, orderRatingResponseDTO);
		Vendor vendor = vendorService.getVendorDetail(orderRating.getVendorId());
		orderRatingResponseDTO.setVendorName(vendor.getFirstName() + " " + vendor.getLastName());
		DeliveryBoy boy = deliveryBoyService.getDeliveryBoyDetail(orderRating.getDeliveryBoyId());
		orderRatingResponseDTO.setDeliveryBoyName(boy.getFirstName().concat(" ").concat(boy.getLastName()));
		return orderRatingResponseDTO;
	}

	public OrderRating toEntity(final OrderRatingDTO orderRatingDTO) {
		OrderRating orderRating = new OrderRating();
		BeanUtils.copyProperties(orderRatingDTO, orderRating);
		return orderRating;
	}

	public List<OrderRatingResponseDTO> toResponseDtos(final List<OrderRating> orderRatingList) throws NotFoundException {
		List<OrderRatingResponseDTO> results = new ArrayList<>();
		for (OrderRating OrderRating : orderRatingList) {
			results.add(toResponseDto(OrderRating));
		}
		return results;
	}
}
