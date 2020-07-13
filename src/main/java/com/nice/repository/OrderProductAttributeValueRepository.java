/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrdersItem;
import com.nice.model.OrdersProductAttributeValue;
import com.nice.model.ProductAttributeValue;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface OrderProductAttributeValueRepository extends JpaRepository<OrdersProductAttributeValue, Long> {

	/**
	 * @param tempOrderItem
	 * @param productAttributeValue
	 * @return
	 */
	Optional<OrdersProductAttributeValue> findAllByOrderItemAndProductAttributeValue(OrdersItem tempOrderItem, ProductAttributeValue productAttributeValue);

	/**
	 * @param tempOrderItem
	 * @return
	 */
	List<OrdersProductAttributeValue> findAllByOrderItem(OrdersItem orderItem);

}
