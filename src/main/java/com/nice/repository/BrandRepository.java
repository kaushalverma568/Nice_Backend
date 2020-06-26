package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Brand;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Repository
public interface BrandRepository extends JpaRepository<Brand, Long> {

	/**
	 * Get Brand by brand name and brand Id not equal if exist
	 *
	 * @param name
	 * @param classification
	 * @param id
	 * @return
	 */
	Optional<Brand> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * Get Brand Page by active
	 *
	 * @param pageable
	 * @param active
	 * @return
	 */
	Page<Brand> findAllByActive(Boolean active, Pageable pageable);

	/**
	 * Get Brand by brand name if exist
	 *
	 * @param name
	 * @param classification
	 * @return
	 */
	Optional<Brand> findByNameIgnoreCase(String name);

	/**
	 * get brand list by active
	 *
	 * @param active
	 * @return
	 */
	List<Brand> findAllByActive(Boolean active);

	/**
	 * get brand list by active and name containing search keyword
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @return
	 */
	List<Brand> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyword);

	/**
	 * get brand list by name containing search keyword
	 *
	 * @param searchKeyword
	 * @return
	 */
	List<Brand> findAllByNameContainingIgnoreCase(String searchKeyword);

	/**
	 * get brand page by active and name containing search keyword
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Brand> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyword, Pageable pageable);

	/**
	 * get brand page by name containing search keyword
	 * 
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Brand> findAllByNameContainingIgnoreCase(String searchKeyword, Pageable pageable);
}
