package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Cuisine;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Repository
public interface CuisineRepository extends JpaRepository<Cuisine, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param searchKeyWord
	 * @param searchKeyWord2
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String searchKeyWord, String searchKeyWord2, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param searchKeyWord
	 * @param activeRecords2
	 * @param searchKeyWord2
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyWord,
			Boolean activeRecords2, String searchKeyWord2, Pageable pageable);

	/**
	 * @param nameEnglish
	 * @param id
	 * @return
	 */
	Optional<Cuisine> findByNameEnglishIgnoreCaseAndIdNot(String nameEnglish, Long id);

	/**
	 * @param nameEnglish
	 * @return
	 */
	Optional<Cuisine> findByNameEnglishIgnoreCase(String nameEnglish);

	/**
	 * @param nameArabic
	 * @param id
	 * @return
	 */
	Optional<Cuisine> findByNameArabicIgnoreCaseAndIdNot(String nameArabic, Long id);

	/**
	 * @param nameArabic
	 * @return
	 */
	Optional<Cuisine> findByNameArabicIgnoreCase(String nameArabic);

	/**
	 * @param cuisineName
	 * @param cuisineName2
	 * @return
	 */
	Optional<Cuisine> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String cuisineName, String cuisineName2);

	/**
	 * @param cuisineNameEnglish
	 * @param cuisineNameArabic
	 * @return
	 */
	Optional<Cuisine> findByNameEnglishIgnoreCaseAndNameArabicIgnoreCase(String cuisineNameEnglish, String cuisineNameArabic);
}
