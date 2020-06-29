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
	 * Get Cuisine by cuisine name if exist
	 *
	 * @param cuisineName
	 * @return
	 */
	Optional<Cuisine> findByNameIgnoreCase(String cuisineName);

	/**
	 * Get Cuisine by cuisine name and cuisine Id not equal if exist
	 *
	 * @param cuisineName
	 * @param cuisineId
	 * @return
	 */

	Optional<Cuisine> findByNameIgnoreCaseAndIdNot(String cuisineName, Long cuisineId);

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByNameContainingIgnoreCase(String searchKeyWord, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<Cuisine> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyWord, Pageable pageable);

}
