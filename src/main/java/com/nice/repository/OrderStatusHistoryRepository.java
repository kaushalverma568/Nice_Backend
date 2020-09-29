/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
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

	/**
	 * 
	 * @param status
	 * @return
	 */
	@Query("select count(*) from OrderStatusHistory osh where osh.status =:status and osh.createdAt = :createdAt" )
	Long countByStatusAndCreatedAt(String status, Date createdAt);
	
	/**
	 * 
	 * @param status
	 * @return
	 */
	@Query("select count(*) from OrderStatusHistory osh inner join Orders o on osh.orderId=o.id where osh.status =:status and osh.createdAt = :createdAt and o.vendor.id = :vendorId ")
	Long countByStatusAndCreatedAtAndVendorId(String status, Date createdAt, Long vendorId);
	
}
