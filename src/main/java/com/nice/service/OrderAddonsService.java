/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.OrderAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrdersItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface OrderAddonsService {

	/**
	 * @param orderAddonsDTO
	 * @param orderItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addOrderAddons(OrderAddonsDTO orderAddonsDTO, OrdersItem orderItem) throws ValidationException, NotFoundException;

	/**
	 * @param orderItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<OrderAddonsDTO> getOrderAddonsListForOrderItem(Long orderItemId) throws NotFoundException;

}
