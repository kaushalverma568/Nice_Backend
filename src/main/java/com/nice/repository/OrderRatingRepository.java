package com.nice.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.OrderRating;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Dec-2019
 */
@Repository
public interface OrderRatingRepository extends JpaRepository<OrderRating, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<OrderRating> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * 
	 * @param orderId
	 * @param id
	 * @return
	 */
	Optional<OrderRating> findByOrderIdAndIdNot(Long orderId, Long id);

	/**
	 * 
	 * @param orderId
	 * @return
	 */
	Optional<OrderRating> findByOrderId(Long orderId);
	
	/**
	 * 
	 * @param deliveryBoyId
	 * @param pageable 
	 * @return
	 */
	List<OrderRating> findByDeliveryBoyId(Long deliveryBoyId, Pageable pageable);

	/**
	 * 
	 * @param vendorId
	 * @return
	 */
	List<OrderRating> findByVendorId(Long vendorId, Pageable pageable);

	/**
	 * 
	 * @param startDate
	 * @param endDate
	 * @return
	 */
	@Query("SELECT e FROM OrderRating e WHERE e.createdAt BETWEEN :startDate AND :endDate")
	List<OrderRating> findByCreatedAtbetween(Date startDate, Date endDate);

	/**
	 * 
	 * @param activeRecords
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<OrderRating> findAllByActiveAndReviewContainingIgnoreCase(Boolean activeRecords, String searchKeyWord,
			Pageable pageable);

	/**
	 * 
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<OrderRating> findAllByReviewContainingIgnoreCase(String searchKeyWord, Pageable pageable);

}
