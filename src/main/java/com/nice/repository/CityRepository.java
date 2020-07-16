package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.City;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Repository(value = "cityRepository")
public interface CityRepository extends JpaRepository<City, Long>, CityCustomRepository {

	/**
	 * Get Page based on active field
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<City> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get City based on name,state and for not given id
	 *
	 * @param  name
	 * @param  state
	 * @param  id
	 * @return
	 */
	Optional<City> findByNameIgnoreCaseAndStateAndIdNot(String name, State state, Long id);

	/**
	 * Get City based on name and state
	 *
	 * @param  name
	 * @param  state
	 * @return
	 */
	Optional<City> findByNameIgnoreCaseAndState(String name, State state);

	/**
	 * 
	 * @param activeRecords
	 * @return
	 */
	List<City> findAllByActive(Boolean activeRecords);

}
