package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Cuisine;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Repository
public interface CuisineRepository extends JpaRepository<Cuisine, Long> {

	/**
	 *
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 *
	 * @param searchKeyWord
	 * @param searchKeyWord2
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String searchKeyWord, String searchKeyWord2, Pageable pageable);

	/**
	 *
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
	 * Get Cuisine by Cuisine english name or arabic name if exist
	 *
	 * @param nameEnglish
	 * @param nameArabic
	 * @return
	 */
	Optional<Cuisine> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String nameEnglish, String nameArabic);

	/**
	 * Get Cuisine by Cuisine english name or arabic name and Cuisine Id not equal
	 * if exist
	 *
	 * @param nameEnglish
	 * @param id
	 * @param nameArabic
	 * @param id2
	 * @return
	 */
	Optional<Cuisine> findByNameEnglishIgnoreCaseAndIdNotOrNameArabicIgnoreCaseAndIdNot(String nameEnglish, Long id, String nameArabic, Long id2);
}
