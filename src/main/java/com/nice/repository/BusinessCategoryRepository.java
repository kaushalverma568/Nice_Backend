package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.BusinessCategory;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface BusinessCategoryRepository extends JpaRepository<BusinessCategory, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<BusinessCategory> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get BusinessCategory by BusinessCategory english name or arabic name if exist
	 *
	 * @param nameEnglish
	 * @param nameArabic
	 * @return
	 */
	Optional<BusinessCategory> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String nameEnglish, String nameArabic);

	/**
	 * Get BusinessCategory by BusinessCategory english name or arabic name and
	 * BusinessCategory Id not equal if exist
	 *
	 * @param nameEnglish
	 * @param id
	 * @param nameArabic
	 * @param id2
	 * @return
	 */
	Optional<BusinessCategory> findByNameEnglishIgnoreCaseAndIdNotOrNameArabicIgnoreCaseAndIdNot(String nameEnglish, Long id, String nameArabic, Long id2);

}
