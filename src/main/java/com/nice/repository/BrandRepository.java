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
	 * @param nameEnglish
	 * @param id
	 * @param nameArabic
	 * @param id2
	 * @return
	 */
	Optional<Brand> findByNameEnglishIgnoreCaseAndIdNotOrNameArabicIgnoreCaseAndIdNot(String nameEnglish, Long id, String nameArabic, Long id2);

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
	 * @param nameEnglish
	 * @param nameArabic
	 * @return
	 */
	Optional<Brand> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String nameEnglish, String nameArabic);

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
	 * @param activeRecords2
	 * @param searchKeyword2
	 * @return
	 */
	List<Brand> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyword,
			Boolean activeRecords2, String searchKeyword2);

	/**
	 * get brand list by name containing search keyword
	 *
	 * @param nameEnglish
	 * @param nameArabic
	 * @return
	 */
	List<Brand> findAllByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String nameEnglish, String nameArabic);

	/**
	 * get brand page by active and name containing search keyword
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param activeRecords2
	 * @param searchKeyword2
	 * @param pageable
	 * @return
	 */
	Page<Brand> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyword,
			Boolean activeRecords2, String searchKeyword2, Pageable pageable);

	/**
	 * get brand page by name containing search keyword
	 *
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param pageable
	 * @return
	 */
	Page<Brand> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyword, String searchKeyword2, Pageable pageable);
}
