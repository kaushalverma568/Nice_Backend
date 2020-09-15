package com.nice.service.impl;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrderLocationService;
import com.nice.service.OrdersService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Jul-2020
 */
@Service("orderLocationService")
@Transactional(rollbackOn = Throwable.class)
public class OrderLocationServiceImpl implements OrderLocationService {

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private OrdersService orderService;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrderLocationMapper orderLocationMapper;

	@Autowired
	private OrderLocationRepository orderLocationRepository;

	@Override
	public void addOrderLocation(final OrderLocationDTO orderLocationDTO) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(orderLocationDTO.getDeliveryBoyId());
		Orders orders = orderService.getOrderById(orderLocationDTO.getOrderId());
		Customer customer = customerService.getCustomerDetails(orderLocationDTO.getCustomerId());
		/**
		 * check if all value matches with each other
		 */
		if (orders.getDeliveryBoy() != null && orders.getDeliveryBoy().getId().equals(deliveryBoy.getId())
				&& orders.getCustomer().getId().equals(customer.getId())) {
			OrderLocation orderLocation = orderLocationMapper.toEntity(orderLocationDTO);
			orderLocation.setActive(true);
			orderLocationRepository.save(orderLocation);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(
					orders.getDeliveryBoy() != null && !orders.getDeliveryBoy().getId().equals(deliveryBoy.getId()) ? "order.delivery.boy.mismatch"
							: "order.customer.mistmatch",
					new Object[] { orderLocationDTO.getOrderId(),
							orders.getDeliveryBoy() != null && !orders.getDeliveryBoy().getId().equals(deliveryBoy.getId())
									? orderLocationDTO.getDeliveryBoyId()
									: orderLocationDTO.getCustomerId() }));
		}
	}

	@Override
	public void deleteLocationsByOrder(final Long orderId) {
		orderLocationRepository.deleteAllByOrderId(orderId);
	}

}
