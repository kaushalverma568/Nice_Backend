/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.CartExtras;
import com.nice.model.CartItem;
import com.nice.model.ProductExtras;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface CartExtrasRepository extends JpaRepository<CartExtras, Long> {

	/**
	 * @param tempCartItem
	 * @param productExtras
	 */
	Optional<CartExtras> findAllByCartItemAndProductExtras(CartItem tempCartItem, ProductExtras productExtras);

	/**
	 * @param tempCartItem
	 * @return
	 */
	List<CartExtras> findAllByCartItem(CartItem tempCartItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByCartItem(CartItem cartItem);

	/**
	 * 
	 * @param productExtras
	 * @return
	 */
	List<CartExtras> findAllByProductExtras(ProductExtras productExtras);

	/**
	 * 
	 * @param productExtras
	 */
	void deleteByProductExtras(ProductExtras productExtras);
}
