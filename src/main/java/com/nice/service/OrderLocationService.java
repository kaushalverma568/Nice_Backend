package com.nice.service;

import com.nice.dto.OrderLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
public interface OrderLocationService {

	void addOrderLocation(OrderLocationDTO orderLocationDTO) throws NotFoundException, ValidationException;

	void deleteLocationsByOrder(Long orderId);
}
