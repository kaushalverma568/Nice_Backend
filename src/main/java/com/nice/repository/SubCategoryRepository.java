package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Category;
import com.nice.model.SubCategory;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Repository
public interface SubCategoryRepository extends JpaRepository<SubCategory, Long> {

	/**
	 * get Page of sub categories by active
	 *
	 * @param pageable
	 * @param activeRecords
	 * @return
	 */
	Page<SubCategory> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * get list of sub category by category and active
	 *
	 * @param category
	 * @param active
	 * @return
	 */
	List<SubCategory> findByCategoryAndActive(Category category, Boolean active);

	/**
	 * Get sub category by sub category name , category and id not equal if exist
	 *
	 * @param name
	 * @param category
	 * @param id
	 * @return
	 */
	Optional<SubCategory> findByNameIgnoreCaseAndCategoryAndIdNot(String name, Category category, Long id);

	/**
	 * Get sub category by sub category name and category if exist
	 *
	 * @param name
	 * @param category
	 * @return
	 */
	Optional<SubCategory> findByNameIgnoreCaseAndCategory(String name, Category category);

	/**
	 * get Page of sub categories by active and category
	 *
	 * @param activeRecords
	 * @param category
	 * @param pageable
	 * @return
	 */
	Page<SubCategory> findAllByActiveAndCategory(Boolean activeRecords, Category category, Pageable pageable);

	/**
	 * get Page of sub categories by category
	 *
	 * @param category
	 * @param pageable
	 * @return
	 */
	Page<SubCategory> findAllByCategory(Category category, Pageable pageable);

	/**
	 * get List of sub categories by category
	 *
	 * @param category
	 * @return
	 */
	List<SubCategory> findAllByCategory(Category category);

	/**
	 * get sub category list by active
	 *
	 * @param active
	 * @return
	 */
	List<SubCategory> findAllByActive(Boolean active);

	/**
	 * get sub category list by active and name containing search keyword
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @return
	 */
	List<SubCategory> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyword);

	/**
	 * get sub category list by name containing search keyword
	 *
	 * @param searchKeyword
	 * @return
	 */
	List<SubCategory> findAllByNameContainingIgnoreCase(String searchKeyword);

}
