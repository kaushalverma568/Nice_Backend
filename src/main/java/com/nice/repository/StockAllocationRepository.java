/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrdersItem;
import com.nice.model.StockAllocation;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Repository(value = "stockAllocationRepository")
public interface StockAllocationRepository extends JpaRepository<StockAllocation, Long> {

	/**
	 * @param orderId
	 */
	List<StockAllocation> findByOrderId(Long orderId);

	/**
	 * @param orderId
	 * @param allocatedFor
	 */
	List<StockAllocation> findAllByOrderIdAndAllocatedFor(Long orderId, String allocatedFor);

	/**
	 * @param orderId
	 * @param allocatedFor
	 * @return
	 */
	List<StockAllocation> findByOrderIdAndAllocatedForIgnoreCase(Long orderId, String allocatedFor);

	/**
	 * @param orderItemId
	 * @param allocatedFor
	 * @return
	 */
	List<StockAllocation> findByOrderItemAndAllocatedForIgnoreCase(OrdersItem orderItem, String allocatedFor);

}
