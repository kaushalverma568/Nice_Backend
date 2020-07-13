/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.OrderProductAttributeValueDTO;
import com.nice.exception.NotFoundException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface OrderProductAttributeValueService {

	/**
	 * @param orderItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<OrderProductAttributeValueDTO> getOrderProductAttributeValueListForOrderItem(Long orderItemId) throws NotFoundException;

}
