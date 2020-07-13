/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrdersExtras;
import com.nice.model.OrdersItem;
import com.nice.model.ProductExtras;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface OrderExtrasRepository extends JpaRepository<OrdersExtras, Long> {

	/**
	 * @param orderItem
	 * @param productExtras
	 */
	Optional<OrdersExtras> findAllByOrderItemAndProductExtras(OrdersItem orderItem, ProductExtras productExtras);

	/**
	 * @param orderItem
	 * @return
	 */
	List<OrdersExtras> findAllByOrderItem(OrdersItem orderItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByOrderItem(OrdersItem orderItem);
}
