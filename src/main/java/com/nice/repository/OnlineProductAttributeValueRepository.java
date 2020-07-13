/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OnlineProductAttributeValue;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Repository
public interface OnlineProductAttributeValueRepository extends JpaRepository<OnlineProductAttributeValue, Long> {

	/**
	 * @param orderItem
	 * @return
	 */
	List<OnlineProductAttributeValue> findAllByOnlineCartId(Long onlineCartId);

}
