package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.DeliveryBoy;
import com.nice.model.OrderRating;
import com.nice.model.Orders;
import com.nice.model.Vendor;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
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

	@Autowired
	private OrdersService ordersService;

	public OrderRatingResponseDTO toResponseDto(final OrderRating orderRating) throws NotFoundException {
		final Locale locale = LocaleContextHolder.getLocale();
		OrderRatingResponseDTO orderRatingResponseDTO = new OrderRatingResponseDTO();
		BeanUtils.copyProperties(orderRating, orderRatingResponseDTO);
		Vendor vendor = vendorService.getVendorDetail(orderRating.getVendorId());
		if (orderRating.getDeliveryBoyId() != null) {
			DeliveryBoy boy = deliveryBoyService.getDeliveryBoyDetail(orderRating.getDeliveryBoyId());
			orderRatingResponseDTO.setDeliveryBoyNameEnglish(boy.getFirstNameEnglish().concat(" ").concat(boy.getLastNameEnglish()));
			orderRatingResponseDTO.setDeliveryBoyNameArabic(boy.getFirstNameArabic().concat(" ").concat(boy.getLastNameArabic()));
			if (locale.getLanguage().equals("en")) {
				orderRatingResponseDTO.setDeliveryBoyName(boy.getFirstNameEnglish().concat(" ").concat(boy.getLastNameEnglish()));
			} else {
				orderRatingResponseDTO.setDeliveryBoyName(boy.getFirstNameArabic().concat(" ").concat(boy.getLastNameArabic()));
			}
		}

		Orders order = ordersService.getOrderById(orderRating.getOrderId());
		orderRatingResponseDTO.setOrderDate(order.getCreatedAt());
		orderRatingResponseDTO.setCustomerName(order.getCustomer().getFirstName() + " " + order.getCustomer().getLastName());
		if (locale.getLanguage().equals("en")) {
			orderRatingResponseDTO.setVendorName(vendor.getStoreNameEnglish());
		} else {
			orderRatingResponseDTO.setVendorName(vendor.getStoreNameArabic());
		}
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
