package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.Category;
import com.nice.model.SubCategory;
import com.nice.model.Vendor;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Repository
public interface SubCategoryRepository extends JpaRepository<SubCategory, Long> {

	/**
	 * get Page of sub categories by active
	 *
	 * @param  pageable
	 * @param  activeRecords
	 * @return
	 */
	Page<SubCategory> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * get list of sub category by category and active
	 *
	 * @param  category
	 * @param  active
	 * @return
	 */
	List<SubCategory> findByCategoryAndActive(Category category, Boolean active);

	/**
	 * get Page of sub categories by active and category
	 *
	 * @param  activeRecords
	 * @param  category
	 * @param  pageable
	 * @return
	 */
	Page<SubCategory> findAllByActiveAndCategory(Boolean activeRecords, Category category, Pageable pageable);

	/**
	 * get Page of sub categories by category
	 *
	 * @param  category
	 * @param  pageable
	 * @return
	 */
	Page<SubCategory> findAllByCategory(Category category, Pageable pageable);

	/**
	 * get List of sub categories by category
	 *
	 * @param  category
	 * @return
	 */
	List<SubCategory> findAllByCategory(Category category);

	/**
	 * get sub category list by active
	 *
	 * @param  active
	 * @return
	 */
	List<SubCategory> findAllByActive(Boolean active);

	/**
	 * get sub category list by active and name containing search keyword
	 *
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  activeRecords2
	 * @param  searchKeyword2
	 * @return
	 */
	List<SubCategory> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyword,
			Boolean activeRecords2, String searchKeyword2);

	/**
	 * get sub category list by name containing search keyword
	 *
	 * @param  searchKeyword
	 * @param  searchKeyword2
	 * @return
	 */
	List<SubCategory> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyword, String searchKeyword2);

	@Query("select sc from SubCategory sc join Category c on c.id=sc.category.id where sc.category.vendor=:vendor")
	List<SubCategory> getAllByVendor(Vendor vendor);

	/**
	 * @param  nameEnglish
	 * @param  category
	 * @param  id
	 * @return
	 */
	Optional<SubCategory> findByNameEnglishIgnoreCaseAndCategoryAndIdNot(String nameEnglish, Category category, Long id);

	/**
	 * @param  nameEnglish
	 * @param  category
	 * @return
	 */
	Optional<SubCategory> findByNameEnglishIgnoreCaseAndCategory(String nameEnglish, Category category);

	/**
	 * @param  nameArabic
	 * @param  category
	 * @param  id
	 * @return
	 */
	Optional<SubCategory> findByNameArabicIgnoreCaseAndCategoryAndIdNot(String nameArabic, Category category, Long id);

	/**
	 * @param  nameArabic
	 * @param  category
	 * @return
	 */
	Optional<SubCategory> findByNameArabicIgnoreCaseAndCategory(String nameArabic, Category category);

	/**
	 * @param  nameEnglish
	 * @param  categoryNameArabic
	 * @param  category
	 * @return
	 */
	Optional<SubCategory> findByNameEnglishIgnoreCaseAndNameArabicIgnoreCaseAndCategory(String nameEnglish, String categoryNameArabic, Category category);

	/**
	 * @param  activeRecords
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<SubCategory> findAllByActiveAndCategoryVendor(Boolean activeRecords, Vendor vendor, Pageable pageable);

	/**
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<SubCategory> findAllByCategoryVendor(Vendor vendor, Pageable pageable);

}
