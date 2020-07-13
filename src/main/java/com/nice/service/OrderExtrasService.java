/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.OrderExtrasDto;
import com.nice.exception.NotFoundException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface OrderExtrasService {

	/**
	 * @param orderItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<OrderExtrasDto> getOrderExtrasListForOrderItem(Long orderItemId) throws NotFoundException;

}
