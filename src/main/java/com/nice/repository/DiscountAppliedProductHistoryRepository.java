package com.nice.repository;

import java.sql.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.DiscountAppliedProductHistory;

@Repository
public interface DiscountAppliedProductHistoryRepository extends JpaRepository<DiscountAppliedProductHistory, Long> {

	Optional<DiscountAppliedProductHistory> findByProductIdAndDiscountId(Long productId, Long discountId);

	List<DiscountAppliedProductHistory> findAllByDiscountId(Long discountId);

	void deleteByProductIdAndDiscountId(Long productId, Long discountId);

	/**
	 * get history of product have a upcoming/active discount and its start date is between new start date & end date or end
	 * date is between new start date & end date
	 *
	 * @param productId
	 * @param disountId
	 * @param startDate1
	 * @param endDate1
	 * @param startDate
	 * @param endDate
	 * @return
	 */
	@Query(value = "select * from discount_applied_product_history where product_id=?1 and discount_id !=?2 and (((start_date,end_date) overlaps (?3,?4)) OR end_date = ?3 OR start_date =?4) and status in('Active','Upcoming')", nativeQuery = true)
	Optional<DiscountAppliedProductHistory> isDiscountExist(Long productId, Long disountId, Date startDate, Date endDate);

}
