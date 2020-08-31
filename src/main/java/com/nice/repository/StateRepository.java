package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Country;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Repository
public interface StateRepository extends JpaRepository<State, Long>, StateCustomRepository {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<State> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get state by state english name or arabic name,country and state Id not equal
	 * if exist
	 *
	 * @param nameEnglish
	 * @param country
	 * @param id
	 * @param nameArabic
	 * @param country2
	 * @param id2
	 * @return
	 */
	Optional<State> findByNameEnglishIgnoreCaseAndCountryAndIdNotOrNameArabicIgnoreCaseAndCountryAndIdNot(String nameEnglish, Country country, Long id,
			String nameArabic, Country country2, Long id2);

	/**
	 * Get state by state english name or arabic name,country and country if exist
	 *
	 * @param nameEnglish
	 * @param country
	 * @param nameArabic
	 * @param country2
	 * @return
	 */
	Optional<State> findByNameEnglishIgnoreCaseAndCountryOrNameArabicIgnoreCaseAndCountry(String nameEnglish, Country country, String nameArabic,
			Country country2);
}
