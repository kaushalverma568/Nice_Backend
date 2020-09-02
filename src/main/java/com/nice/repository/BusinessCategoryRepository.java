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
	 * @return
	 */
	Optional<BusinessCategory> findByNameEnglishIgnoreCase(String nameEnglish);

	/**
	 * Get BusinessCategory by BusinessCategory english name or arabic name and BusinessCategory Id not equal if exist
	 *
	 * @param nameEnglish
	 * @param id
	 * @return
	 */
	Optional<BusinessCategory> findByNameEnglishIgnoreCaseAndIdNot(String nameEnglish, Long id);

	/**
	 * @param nameArabic
	 * @param id
	 * @return
	 */
	Optional<BusinessCategory> findByNameArabicIgnoreCaseAndIdNot(String nameArabic, Long id);

	/**
	 * @param nameArabic
	 * @return
	 */
	Optional<BusinessCategory> findByNameArabicIgnoreCase(String nameArabic);

}
