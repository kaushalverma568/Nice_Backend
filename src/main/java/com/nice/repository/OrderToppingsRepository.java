/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrdersItem;
import com.nice.model.OrdersToppings;
import com.nice.model.ProductTopping;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface OrderToppingsRepository extends JpaRepository<OrdersToppings, Long> {

	/**
	 *
	 * @param tempOrderItem
	 * @param productToppings
	 * @return
	 */
	Optional<OrdersToppings> findAllByOrderItemAndProductToppings(OrdersItem tempOrderItem, ProductTopping productToppings);

	/**
	 * @param orderItem
	 * @return
	 */
	List<OrdersToppings> findAllByOrderItem(OrdersItem orderItem);

}
