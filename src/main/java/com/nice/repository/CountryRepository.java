package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Country;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Repository(value = "countryRepository")
public interface CountryRepository extends JpaRepository<Country, Long> {

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Country> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get page of country by active and (english name or arabic name) contains search keyword
	 *
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @param  activeRecords2
	 * @param  searchKeyWord2
	 * @param  pageable
	 * @return
	 */
	Page<Country> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyWord,
			Boolean activeRecords2, String searchKeyWord2, Pageable pageable);

	/**
	 * Get page of country by english name or arabic name contains search keyword
	 *
	 * @param  searchKeyWord
	 * @param  searchKeyWord2
	 * @param  pageable
	 * @return
	 */
	Page<Country> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyWord, String searchKeyWord2, Pageable pageable);

	/**
	 * Get Country by country with same name(English) and not same Id
	 *
	 * @param  nameEnglish
	 * @param  id
	 * @return
	 */
	Optional<Country> findByNameEnglishIgnoreCaseAndIdNot(String nameEnglish, Long id);

	/**
	 * Get Country by country with same name(English)
	 *
	 * @param  nameEnglish
	 * @return
	 */
	Optional<Country> findByNameEnglishIgnoreCase(String nameEnglish);

	/**
	 * Get Country by country with same name(Arabic) and not same Id
	 *
	 * @param  nameArabic
	 * @param  id
	 * @return
	 */
	Optional<Country> findByNameArabicIgnoreCaseAndIdNot(String nameArabic, Long id);

	/**
	 * Get Country by country with same name(Arabic)
	 *
	 * @param  nameArabic
	 * @return
	 */
	Optional<Country> findByNameArabicIgnoreCase(String nameArabic);

	/**
	 * @param  countryNameEnglish
	 * @param  countryNameArabic
	 * @return
	 */
	Optional<Country> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String countryNameEnglish, String countryNameArabic);
}
