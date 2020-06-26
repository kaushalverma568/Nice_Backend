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
	 * Get Country by country name if exist
	 *
	 * @param  countryName
	 * @return
	 */
	Optional<Country> findByNameIgnoreCase(String countryName);

	/**
	 * Get Country by country name and country Id not equal if exist
	 *
	 * @param  countryName
	 * @param  countryId
	 * @return
	 */

	Optional<Country> findByNameIgnoreCaseAndIdNot(String countryName, Long countryId);

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Country> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param  searchKeyWord
	 * @param  pageable
	 * @return
	 */
	Page<Country> findAllByNameContainingIgnoreCase(String searchKeyWord, Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @param  pageable
	 * @return
	 */
	Page<Country> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyWord, Pageable pageable);

}
