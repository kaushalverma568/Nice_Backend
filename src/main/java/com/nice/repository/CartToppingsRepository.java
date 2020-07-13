/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.CartItem;
import com.nice.model.CartToppings;
import com.nice.model.ProductTopping;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface CartToppingsRepository extends JpaRepository<CartToppings, Long> {

	/**
	 *
	 * @param tempCartItem
	 * @param productToppings
	 * @return
	 */
	Optional<CartToppings> findAllByCartItemAndProductToppings(CartItem tempCartItem, ProductTopping productToppings);

	/**
	 * @param tempCartItem
	 * @return
	 */
	List<CartToppings> findAllByCartItem(CartItem tempCartItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByCartItem(CartItem cartItem);

}
