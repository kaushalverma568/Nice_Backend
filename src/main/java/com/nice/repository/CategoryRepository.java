package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Category;
import com.nice.model.Vendor;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

	/**
	 * Get category by category name, category Id not equal and vendor if exist
	 *
	 * @param name
	 * @param vendor
	 * @param id
	 * @return
	 */
	Optional<Category> findByNameIgnoreCaseAndVendorAndIdNot(String name, Vendor vendor, Long id);

	/**
	 * Get category Page by active
	 *
	 * @param pageable
	 * @param active
	 * @return
	 */
	Page<Category> findAllByActive(Boolean active, Pageable pageable);

	/**
	 * Get category by category name and vendor if exist
	 *
	 * @param name
	 * @return
	 */
	Optional<Category> findByNameIgnoreCaseAndVendor(String name, Vendor vendor);

	/**
	 * get category page name containing search keyword
	 *
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByNameContainingIgnoreCase(String searchKeyword, Pageable pageable);

	/**
	 * get category page name containing search keyword andactive
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyword, Pageable pageable);

	/**
	 * get category list by vendor
	 *
	 * @param vendor
	 * @return
	 */
	List<Category> findAllByVendor(Vendor vendor);

	/**
	 * @param searchKeyword
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByNameContainingIgnoreCaseAndVendor(String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByVendor(Vendor vendor, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param searchKeyword
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndNameContainingIgnoreCaseAndVendor(Boolean activeRecords, String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndVendor(Boolean activeRecords, Vendor vendor, Pageable pageable);
}
