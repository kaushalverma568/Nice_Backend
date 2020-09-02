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
 * @date   : 22-Jun-2020
 */
@Repository
public interface StateRepository extends JpaRepository<State, Long>, StateCustomRepository {

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<State> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get State by name(English) with same Country and not same Id
	 *
	 * @param  nameEnglish
	 * @param  country
	 * @param  id
	 * @return
	 */
	Optional<State> findByNameEnglishIgnoreCaseAndCountryAndIdNot(String nameEnglish, Country country, Long id);

	/**
	 * Get State by name(English) with same Country
	 *
	 * @param  nameEnglish
	 * @param  country
	 * @return
	 */
	Optional<State> findByNameEnglishIgnoreCaseAndCountry(String nameEnglish, Country country);

	/**
	 * Get State by name(Arabic) with same Country and not same Id
	 *
	 * @param  nameArabic
	 * @param  country
	 * @param  id
	 * @return
	 */
	Optional<State> findByNameArabicIgnoreCaseAndCountryAndIdNot(String nameArabic, Country country, Long id);

	/**
	 * Get State by name(Arabic) with same Country
	 *
	 * @param  nameArabic
	 * @param  country
	 * @return
	 */
	Optional<State> findByNameArabicIgnoreCaseAndCountry(String nameArabic, Country country);

	/**
	 * @param  nameEnglish
	 * @param  country
	 * @param  nameArabic
	 * @param  country2
	 * @return
	 */
	Optional<State> findByNameEnglishIgnoreCaseAndCountryOrNameArabicIgnoreCaseAndCountry(String nameEnglish, Country country, String nameArabic,
			Country country2);
}
