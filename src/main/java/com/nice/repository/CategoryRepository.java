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
	 * Get category Page by active
	 *
	 * @param pageable
	 * @param active
	 * @return
	 */
	Page<Category> findAllByActive(Boolean active, Pageable pageable);

	/**
	 * get category page name containing search keyword
	 *
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyword, String searchKeyword2, Pageable pageable);

	/**
	 * get category page name containing search keyword andactive
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyword,
			Boolean activeRecords2, String searchKeyword2, Pageable pageable);

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
	Page<Category> findAllByNameEnglishContainingIgnoreCaseAndVendorOrNameArabicContainingIgnoreCaseAndVendor(String searchKeyword, Vendor vendor,
			String searchKeyword2, Vendor vendor2, Pageable pageable);

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
	Page<Category> findAllByActiveAndNameEnglishContainingIgnoreCaseAndVendorOrActiveAndNameArabicContainingIgnoreCaseAndVendor(Boolean activeRecords,
			String searchKeyword, Vendor vendor, Boolean activeRecords2, String searchKeyword2, Vendor vendor2, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndVendor(Boolean activeRecords, Vendor vendor, Pageable pageable);

	/**
	 * Get Category based on english name or arabic name ,vendor and for not given
	 * id
	 *
	 * @param nameEnglish
	 * @param vendor
	 * @param id
	 * @param nameArabic
	 * @param vendor2
	 * @param id2
	 * @return
	 */
	Optional<Category> findByNameEnglishIgnoreCaseAndVendorAndIdNotOrNameArabicIgnoreCaseAndVendorAndIdNot(String nameEnglish, Vendor vendor, Long id,
			String nameArabic, Vendor vendor2, Long id2);

	/**
	 * Get Category based on english name or arabic name and vendor
	 *
	 * @param nameEnglish
	 * @param vendor
	 * @param nameArabic
	 * @param vendor2
	 * @return
	 */
	Optional<Category> findByNameEnglishIgnoreCaseAndVendorOrNameArabicIgnoreCaseAndVendor(String nameEnglish, Vendor vendor, String nameArabic,
			Vendor vendor2);
}
