package com.nice.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Discount;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Repository
public interface DiscountRepository extends JpaRepository<Discount, Long> {

	/**
	 * get all discount by status
	 *
	 * @param  status
	 * @param  pageable
	 * @return
	 */
	Page<Discount> findAllByStatus(String status, Pageable pageable);

	/**
	 * get all discount by category id and status list
	 *
	 * @param  categoryId
	 * @param  asList
	 * @return
	 */
	List<Discount> findAllByCategoryIdAndStatusIn(Long categoryId, List<String> asList);

	/**
	 * get all discount by vendor id
	 *
	 * @param  vendorId
	 * @param  pageable
	 * @return
	 */
	Page<Discount> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 * get all discount by status and vendor id
	 *
	 * @param  status
	 * @param  vendorId
	 * @param  pageable
	 * @return
	 */
	Page<Discount> findAllByStatusAndVendorId(String status, Long vendorId, Pageable pageable);
}
