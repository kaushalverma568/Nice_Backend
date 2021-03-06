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
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

	/**
	 * Get category Page by active
	 *
	 * @param  pageable
	 * @param  active
	 * @return
	 */
	Page<Category> findAllByActive(Boolean active, Pageable pageable);

	/**
	 * get category page by name containing search keyword
	 *
	 * @param  searchKeyword
	 * @param  pageable
	 * @return
	 */
	Page<Category> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyword, String searchKeyword2, Pageable pageable);

	/**
	 * get category page by name(english,arabic) containing search keyword and active
	 *
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyword,
			Boolean activeRecords2, String searchKeyword2, Pageable pageable);

	/**
	 * get category list by vendor
	 *
	 * @param  vendor
	 * @return
	 */
	List<Category> findAllByVendor(Vendor vendor);

	/**
	 * get category page by name(english,arabic) containing search keyword and vendor
	 *
	 * @param  searchKeyword
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Category> findAllByNameEnglishContainingIgnoreCaseAndVendorOrNameArabicContainingIgnoreCaseAndVendor(String searchKeyword, Vendor vendor,
			String searchKeyword2, Vendor vendor2, Pageable pageable);

	/**
	 * get category page by vendor
	 *
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Category> findAllByVendor(Vendor vendor, Pageable pageable);

	/**
	 * get category page by name(english,arabic) containing search keyword,active and vendor
	 *
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndNameEnglishContainingIgnoreCaseAndVendorOrActiveAndNameArabicContainingIgnoreCaseAndVendor(Boolean activeRecords,
			String searchKeyword, Vendor vendor, Boolean activeRecords2, String searchKeyword2, Vendor vendor2, Pageable pageable);

	/**
	 * get category page by active and vendor
	 *
	 * @param  activeRecords
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Category> findAllByActiveAndVendor(Boolean activeRecords, Vendor vendor, Pageable pageable);

	/**
	 * getcategory by name english, vendor and id not if exist
	 *
	 * @param  vendor
	 * @param  id
	 * @return
	 */
	Optional<Category> findByNameEnglishIgnoreCaseAndVendorAndIdNot(String nameEnglish, Vendor vendor, Long id);

	/**
	 * get category by name english and vendor if exist
	 *
	 * @param  nameEnglish
	 * @param  vendor
	 * @return
	 */
	Optional<Category> findByNameEnglishIgnoreCaseAndVendor(String nameEnglish, Vendor vendor);

	/**
	 * get category by name arabic, vendor and id not if exist
	 *
	 * @param  nameArabic
	 * @param  vendor
	 * @param  id
	 * @return
	 */
	Optional<Category> findByNameArabicIgnoreCaseAndVendorAndIdNot(String nameArabic, Vendor vendor, Long id);

	/**
	 * get category by name arabic, vendor and id not if exist
	 *
	 * @param  nameArabic
	 * @param  vendor
	 * @return
	 */
	Optional<Category> findByNameArabicIgnoreCaseAndVendor(String nameArabic, Vendor vendor);

	/**
	 * get category by name (english,arabic) and vendor if exist
	 *
	 * @param  nameEnglish
	 * @param  nameArabic
	 * @param  vendor
	 * @return
	 */
	Optional<Category> findByNameEnglishIgnoreCaseAndNameArabicIgnoreCaseAndVendor(String nameEnglish, String nameArabic, Vendor vendor);
}
