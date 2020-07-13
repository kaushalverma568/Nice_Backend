/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OnlineToppings;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Repository
public interface OnlineToppingsRepository extends JpaRepository<OnlineToppings, Long> {

	/**
	 * @param orderItem
	 * @return
	 */
	List<OnlineToppings> findAllByOnlineCartId(Long onlineCartId);

}
