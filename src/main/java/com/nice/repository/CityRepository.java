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
	 * @param  activeRecords
	 * @return
	 */
	List<City> findAllByActive(Boolean activeRecords);

	/**
	 * Get City based on english name , state and for not given id
	 *
	 * @param  nameEnglish
	 * @param  state
	 * @param  id
	 * @param  nameArabic
	 * @param  state2
	 * @param  id2
	 * @return
	 */
	Optional<City> findByNameEnglishIgnoreCaseAndStateAndIdNot(String nameEnglish, State state, Long id);

	/**
	 * Get City based on arabic name, state and not given id
	 *
	 * @param  nameArabic
	 * @param  state
	 * @param  id
	 * @return
	 */
	Optional<City> findByNameArabicIgnoreCaseAndStateAndIdNot(String nameArabic, State state, Long id);

	/**
	 * Get City based on english name and state
	 *
	 * @param  nameEnglish
	 * @param  state
	 * @param  nameArabic
	 * @param  state2
	 * @return
	 */
	Optional<City> findByNameEnglishIgnoreCaseAndState(String nameEnglish, State state);

	/**
	 * Get City based on arabic name and state
	 *
	 * @param  nameEnglish
	 * @param  state
	 * @return
	 */
	Optional<City> findByNameArabicIgnoreCaseAndState(String nameArabic, State state);

	/**
	 * get city based on english or arabic name
	 *
	 * @param  nameEnglish
	 * @param  nameArabic
	 * @return
	 */
	Optional<City> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String nameEnglish, String nameArabic);

}
