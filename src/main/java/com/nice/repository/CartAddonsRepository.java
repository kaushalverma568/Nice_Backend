/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.CartAddons;
import com.nice.model.CartItem;
import com.nice.model.ProductAddons;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 05-Jul-2020
 */
@Repository
public interface CartAddonsRepository extends JpaRepository<CartAddons, Long> {

	/**
	 * @param tempCartItem
	 * @param productAddons
	 */
	Optional<CartAddons> findAllByCartItemAndProductAddons(CartItem tempCartItem, ProductAddons productAddons);

	/**
	 * @param  tempCartItem
	 * @return
	 */
	List<CartAddons> findAllByCartItem(CartItem tempCartItem);

	/**
	 * @param  productAddons
	 * @return
	 */
	List<CartAddons> findAllByProductAddons(ProductAddons productAddons);

	/**
	 * @param cartItem
	 */
	void deleteAllByCartItem(CartItem cartItem);

}
