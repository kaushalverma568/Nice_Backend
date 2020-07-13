/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OnlineAddons;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Repository
public interface OnlineAddonsRepository extends JpaRepository<OnlineAddons, Long> {

	/**
	 * @param orderItem
	 * @return
	 */
	List<OnlineAddons> findAllByOnlineCartId(Long onlineCartId);

}
