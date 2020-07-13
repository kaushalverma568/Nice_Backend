/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.OrderToppingsDto;
import com.nice.exception.NotFoundException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface OrderToppingsService {

	/**
	 * @param orderItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<OrderToppingsDto> getOrderToppingsListForOrderItem(Long orderItemId) throws NotFoundException;

}
