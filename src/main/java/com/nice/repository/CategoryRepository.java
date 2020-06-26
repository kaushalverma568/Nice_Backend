package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Category;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

	/**
	 * Get category by category name and category Id not equal if exist
	 *
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<Category> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * Get category Page by active
	 *
	 * @param pageable
	 * @param active
	 * @return
	 */
	Page<Category> findAllByActive(Boolean active, Pageable pageable);

	/**
	 * Get category by category name if exist
	 *
	 * @param name
	 * @return
	 */
	Optional<Category> findByNameIgnoreCase(String name);

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
}
