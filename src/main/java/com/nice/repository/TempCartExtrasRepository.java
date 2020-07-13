/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductExtras;
import com.nice.model.TempCartExtras;
import com.nice.model.TempCartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface TempCartExtrasRepository extends JpaRepository<TempCartExtras, Long> {

	/**
	 * @param tempCartItem
	 * @param productExtras
	 */
	Optional<TempCartExtras> findAllByTempCartItemAndProductExtras(TempCartItem tempCartItem, ProductExtras productExtras);

	/**
	 * @param tempCartItem
	 * @return
	 */
	List<TempCartExtras> findAllByTempCartItem(TempCartItem tempCartItem);

	/**
	 * @param cartItem
	 */
	void deleteAllByTempCartItem(TempCartItem cartItem);
}
