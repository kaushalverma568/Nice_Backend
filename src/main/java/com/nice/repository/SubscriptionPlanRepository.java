package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.SubscriptionPlan;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Repository
public interface SubscriptionPlanRepository extends JpaRepository<SubscriptionPlan, Long> {

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<SubscriptionPlan> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param  searchKeyword
	 * @param  searchKeyword2
	 * @param  pageable
	 * @return
	 */
	Page<SubscriptionPlan> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyword, String searchKeyword2,
			Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  activeRecords2
	 * @param  searchKeyword2
	 * @param  pageable
	 * @return
	 */
	Page<SubscriptionPlan> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords,
			String searchKeyword, Boolean activeRecords2, String searchKeyword2, Pageable pageable);

	/**
	 * @param  days
	 * @param  id
	 * @return
	 */
	Optional<SubscriptionPlan> findByDaysAndIdNot(Integer days, Long id);

	/**
	 * @param  days
	 * @return
	 */
	Optional<SubscriptionPlan> findByDays(Integer days);

	/**
	 * @param  nameEnglish
	 * @param  id
	 * @return
	 */
	Optional<SubscriptionPlan> findByNameEnglishIgnoreCaseAndIdNot(String nameEnglish, Long id);

	/**
	 * @param  nameEnglish
	 * @return
	 */
	Optional<SubscriptionPlan> findByNameEnglishIgnoreCase(String nameEnglish);

	/**
	 * @param  nameArabic
	 * @param  id
	 * @return
	 */
	Optional<SubscriptionPlan> findByNameArabicIgnoreCaseAndIdNot(String nameArabic, Long id);

	/**
	 * @param  nameArabic
	 * @return
	 */
	Optional<SubscriptionPlan> findByNameArabicIgnoreCase(String nameArabic);

}
