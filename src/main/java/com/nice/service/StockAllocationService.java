/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.StockAllocationDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.StockAllocation;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
public interface StockAllocationService {

	/**
	 * @param stockAllocationDto
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long allocateStock(StockAllocationDto stockAllocationDto, Long userId) throws NotFoundException, ValidationException;

	/**
	 * @param orderId
	 * @param allocatedFor
	 * @return
	 */
	List<StockAllocation> getAllocatedStockForOrder(Long orderId, String allocatedFor);

	/**
	 * @param orderItemId
	 * @param allocatedFor
	 * @return
	 * @throws NotFoundException
	 */
	List<StockAllocation> getAllocatedStockForOrderItem(Long orderItemId, String allocatedFor) throws NotFoundException;

	void sendEmailOnOrderStatusChange(Long orderId, String inProcessOrder) throws NotFoundException;
}
