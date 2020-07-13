/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OnlineExtras;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Repository
public interface OnlineExtrasRepository extends JpaRepository<OnlineExtras, Long> {

	/**
	 * @param orderItem
	 * @return
	 */
	List<OnlineExtras> findAllByOnlineCartId(Long onlineCartId);

}
