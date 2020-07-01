package com.nice.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrderItemRating;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Dec-2019
 */
@Repository
public interface OrderItemRatingRepository extends JpaRepository<OrderItemRating, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<OrderItemRating> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * 
	 * @param productId
	 * @return
	 */
	List<OrderItemRating> findByProductId(Long productId);

	/**
	 * 
	 * @param orderRatingId
	 * @return
	 */
	List<OrderItemRating> findByOrderRatingId(Long orderRatingId);

}
