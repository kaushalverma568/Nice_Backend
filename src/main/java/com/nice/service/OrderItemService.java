/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.OrderItemResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrdersItem;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
public interface OrderItemService {

	/**
	 * @param orderItemId
	 * @return
	 * @throws NotFoundException
	 */
	OrdersItem getOrderItemDetails(Long orderItemId) throws NotFoundException;

	/**
	 *
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 */
	List<OrdersItem> getOrderItemForOrderId(Long orderId) throws NotFoundException;

	/**
	 * @param orderItemList
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<OrderItemResponseDTO> toOrderItemResponseDto(List<OrdersItem> orderItemList) throws NotFoundException, ValidationException;

	/**
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<OrderItemResponseDTO> getOrderItemResponseDTOForOrderId(Long orderId) throws NotFoundException, ValidationException;

	/**
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 */
	List<OrdersItem> getOrderItemForReplacementOrderId(Long orderId) throws NotFoundException;

}
