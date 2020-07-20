/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrderStatusHistory;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
@Repository
public interface OrderStatusHistoryRepository extends JpaRepository<OrderStatusHistory, Long> {

	/**
	 * @param id
	 */
	List<OrderStatusHistory> findAllByOrderId(Long id);

	/**
	 * @param id
	 */
	Optional<OrderStatusHistory> findByOrderIdAndStatus(Long id, String status);
}
