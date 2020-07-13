/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductTopping;
import com.nice.model.TempCartItem;
import com.nice.model.TempCartToppings;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface TempCartToppingsRepository extends JpaRepository<TempCartToppings, Long> {

	/**
	 *
	 * @param tempCartItem
	 * @param productToppings
	 * @return
	 */
	Optional<TempCartToppings> findAllByTempCartItemAndProductToppings(TempCartItem tempCartItem, ProductTopping productToppings);

	/**
	 * @param tempCartItem
	 * @return
	 */
	List<TempCartToppings> findAllByTempCartItem(TempCartItem tempCartItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByTempCartItem(TempCartItem cartItem);

}
