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
 * @date : 22-Jun-2020
 */
@Repository(value = "cityRepository")
public interface CityRepository extends JpaRepository<City, Long>, CityCustomRepository {

	/**
	 * Get Page based on active field
	 *
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<City> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 *
	 * @param activeRecords
	 * @return
	 */
	List<City> findAllByActive(Boolean activeRecords);

	/**
	 * Get City based on english name or arabic name ,state and for not given id
	 *
	 * @param nameEnglish
	 * @param state
	 * @param id
	 * @param nameArabic
	 * @param state2
	 * @param id2
	 * @return
	 */
	Optional<City> findByNameEnglishIgnoreCaseAndStateAndIdNotOrNameArabicIgnoreCaseAndStateAndIdNot(String nameEnglish, State state, Long id,
			String nameArabic, State state2, Long id2);

	/**
	 * Get City based on english name or arabic name and state
	 *
	 * @param nameEnglish
	 * @param state
	 * @param nameArabic
	 * @param state2
	 * @return
	 */
	Optional<City> findByNameEnglishIgnoreCaseAndStateOrNameArabicIgnoreCaseAndState(String nameEnglish, State state, String nameArabic, State state2);

}
