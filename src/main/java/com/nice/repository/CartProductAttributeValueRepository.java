/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.CartItem;
import com.nice.model.CartProductAttributeValue;
import com.nice.model.ProductAttributeValue;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface CartProductAttributeValueRepository extends JpaRepository<CartProductAttributeValue, Long> {

	/**
	 * @param cartItem
	 */
	void deleteAllByCartItem(CartItem cartItem);

	/**
	 * @param tempCartItem
	 * @param productAttributeValue
	 * @return
	 */
	Optional<CartProductAttributeValue> findAllByCartItemAndProductAttributeValue(CartItem tempCartItem, ProductAttributeValue productAttributeValue);

	/**
	 * @param tempCartItem
	 * @return
	 */
	List<CartProductAttributeValue> findAllByCartItem(CartItem tempCartItem);

}
