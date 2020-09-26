/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.OrdersItem;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
@Repository
public interface OrderItemRepository extends JpaRepository<OrdersItem, Long> {

	/**
	 * @param orderId
	 */
	List<OrdersItem> findAllByOrderId(Long orderId);

	/**
	 * @param orderId
	 * @param b
	 * @return
	 */
	List<OrdersItem> findAllByOrderIdAndReplaceRequested(Long orderId, boolean b);

	/**
	 * @param id
	 * @return
	 */
	@Query("Select sum(quantity) from OrdersItem oi where oi.order.id=:id group by oi.order.id")
	Long getTotalItemCountForOrder(Long id);

}
