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
	 * @param  name
	 * @param  country
	 * @param  id
	 * @return
	 */
	Optional<State> findByNameIgnoreCaseAndCountryAndIdNot(String name, Country country, Long id);

	/**
	 * @param  name
	 * @param  country
	 * @return
	 */
	Optional<State> findByNameIgnoreCaseAndCountry(String name, Country country);

}
