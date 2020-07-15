/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductAddons;
import com.nice.model.TempCartAddons;
import com.nice.model.TempCartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 05-Jul-2020
 */
@Repository
public interface TempCartAddonsRepository extends JpaRepository<TempCartAddons, Long> {

	/**
	 * @param tempCartItem
	 * @param productAddons
	 */
	Optional<TempCartAddons> findAllByTempCartItemAndProductAddons(TempCartItem tempCartItem, ProductAddons productAddons);

	/**
	 * @param  tempCartItem
	 * @return
	 */
	List<TempCartAddons> findAllByTempCartItem(TempCartItem tempCartItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByTempCartItem(TempCartItem cartItem);

	/**
	 * @param  productAddons
	 * @return
	 */
	List<TempCartAddons> findAllByProductAddons(ProductAddons productAddons);
}
