/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.dto.OrderListFilterDto;
import com.nice.model.Orders;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 13-Apr-2020
 */
public interface OrderCustomRepository {

	/**
	 * @param orderListFilterDto
	 * @return
	 */
	Long getOrderCountBasedOnParams(OrderListFilterDto orderListFilterDto);

	/**
	 * @param startIndex
	 * @param pageSize
	 * @param orderListFilterDto
	 * @return
	 */
	List<Orders> getOrderListBasedOnParams(Integer startIndex, Integer pageSize, OrderListFilterDto orderListFilterDto);

	/**
	 * @param deliveryBoyId
	 * @return
	 */
	// Double getTotalCashCollectionByDeliveryBoyForToday(Long deliveryBoyId);

	/**
	 * get sales report
	 *
	 * @param year
	 * @param storeId
	 * @return
	 * @throws ValidationException
	 */
	// SalesReportDto getSalesReport(Integer year, Long storeId, String orderType) throws ValidationException;
}
