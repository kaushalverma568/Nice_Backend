package com.nice.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Discount;

@Repository
public interface DiscountRepository extends JpaRepository<Discount, Long> {

	/**
	 * get all discount by status and category id
	 * 
	 * @param status
	 * @param categoryId
	 * @param pageable
	 * @return
	 */
	Page<Discount> findAllByStatusAndCategoryId(String status, Long categoryId, Pageable pageable);

	/**
	 * get all discount by status
	 * 
	 * @param status
	 * @param pageable
	 * @return
	 */
	Page<Discount> findAllByStatus(String status, Pageable pageable);

	/**
	 * get all discount by category Id
	 * 
	 * @param brandId
	 * @param pageable
	 * @return
	 */
	Page<Discount> findAllByCategoryId(Long categoryId, Pageable pageable);

	/**
	 * get all discount by category id and status list
	 * @param categoryId
	 * @param asList
	 * @return
	 */
	List<Discount> findAllByCategoryIdAndStatusIn(Long categoryId, List<String> asList);
}
