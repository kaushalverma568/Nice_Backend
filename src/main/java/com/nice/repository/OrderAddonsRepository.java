/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrdersAddons;
import com.nice.model.OrdersItem;
import com.nice.model.ProductAddons;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface OrderAddonsRepository extends JpaRepository<OrdersAddons, Long> {

	/**
	 * @param orderItem
	 * @param productAddons
	 */
	Optional<OrdersAddons> findAllByOrderItemAndProductAddons(OrdersItem orderItem, ProductAddons productAddons);

	/**
	 * @param orderItem
	 * @return
	 */
	List<OrdersAddons> findAllByOrderItem(OrdersItem orderItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByOrderItem(OrdersItem cartItem);

}
