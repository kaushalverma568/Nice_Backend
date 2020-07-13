/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OnlineCart;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Repository(value = "onlineCartRepository")
public interface OnlineCartRepository extends JpaRepository<OnlineCart, Long> {

	/**
	 * get all by onlineOrderId
	 *
	 * @param orderId
	 * @param status
	 * @return
	 */
	List<OnlineCart> findAllByOnlineOrderIdAndStatus(String onlineOrderId, String status);

}
