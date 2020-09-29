package com.nice.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderLocationMapper;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.OrderLocation;
import com.nice.model.Orders;
import com.nice.repository.OrderLocationRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrderLocationService;
import com.nice.service.OrdersService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
@Service("orderLocationService")
@Transactional(rollbackFor = Throwable.class)
public class OrderLocationServiceImpl implements OrderLocationService {

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private OrdersService orderService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrderLocationMapper orderLocationMapper;

	@Autowired
	private OrderLocationRepository orderLocationRepository;

	@Override
	public OrderLocation addOrderLocation(final OrderLocationDTO orderLocationDTO) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(orderLocationDTO.getDeliveryBoyId());
		Orders orders = orderService.getOrderById(orderLocationDTO.getOrderId());
		Customer customer = orders.getCustomer();
		orderLocationDTO.setCustomerId(customer.getId());
		/**
		 * check if all value matches with each other
		 */
		if (orders.getDeliveryBoy() != null && orders.getDeliveryBoy().getId().equals(deliveryBoy.getId())) {
			OrderLocation orderLocation = orderLocationMapper.toEntity(orderLocationDTO);
			orderLocation.setActive(true);
			orderLocationRepository.save(orderLocation);
			return orderLocation;
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("order.delivery.boy.mismatch",
					new Object[] { orderLocationDTO.getOrderId(), orderLocationDTO.getDeliveryBoyId() }));
		}
	}

	@Override
	public void deleteLocationsByOrder(final Long orderId) {
		orderLocationRepository.deleteAllByOrderId(orderId);
	}

}
