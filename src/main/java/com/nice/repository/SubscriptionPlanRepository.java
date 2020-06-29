package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.SubscriptionPlan;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface SubscriptionPlanRepository extends JpaRepository<SubscriptionPlan, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<SubscriptionPlan> findAllByActive(Boolean activeRecords, Pageable pageable);
    
	/**
	 * 
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<SubscriptionPlan> findAllByNameContainingIgnoreCase(String searchKeyWord, Pageable pageable);

	/**
	 * 
	 * @param activeRecords
	 * @param searchKeyWord
	 * @param pageable
	 * @return
	 */
	Page<SubscriptionPlan> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyWord,
			Pageable pageable);

	/**
	 * 
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<SubscriptionPlan> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * 
	 * @param name
	 * @return
	 */
	Optional<SubscriptionPlan>findByNameIgnoreCase(String name);

}
