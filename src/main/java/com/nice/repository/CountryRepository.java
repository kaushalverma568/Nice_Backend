package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Country;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Repository(value = "countryRepository")
public interface CountryRepository extends JpaRepository<Country, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Country> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<Country> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyWord, String searchKeyWord2, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<Country> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyWord,
			Boolean activeRecords2, String searchKeyWord2, Pageable pageable);

	/**
	 * Get Country by country english name or arabic name if exist
	 *
	 * @param nameEnglish
	 * @param nameArabic
	 * @return
	 */
	Optional<Country> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String nameEnglish, String nameArabic);

	/**
	 * Get Country by country english name or arabic name and country Id not equal
	 * if exist
	 *
	 * @param nameEnglish
	 * @param id
	 * @param nameArabic
	 * @param id2
	 * @return
	 */
	Optional<Country> findByNameEnglishIgnoreCaseAndIdNotOrNameArabicIgnoreCaseAndIdNot(String nameEnglish, Long id, String nameArabic, Long id2);

}
