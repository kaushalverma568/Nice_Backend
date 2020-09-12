package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Order;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.RatingQuestion;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Repository
public interface RatingQuestionRepository extends JpaRepository<RatingQuestion, Long> {

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<RatingQuestion> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param  questionEnglish
	 * @param  id
	 * @return
	 */
	Optional<Order> findByQuestionEnglishIgnoreCaseAndIdNot(String questionEnglish, Long id);

	/**
	 * @param  questionEnglish
	 * @return
	 */
	Optional<Order> findByQuestionEnglishIgnoreCase(String questionEnglish);

	/**
	 * @param  questionArabic
	 * @param  id
	 * @return
	 */
	Optional<Order> findByQuestionArabicIgnoreCaseAndIdNot(String questionArabic, Long id);

	/**
	 * @param  questionArabic
	 * @return
	 */
	Optional<Order> findByQuestionArabicIgnoreCase(String questionArabic);

	/***
	 * 
	 * @param type
	 * @param pageable
	 * @return
	 */
	Page<RatingQuestion> findAllByType(String type, Pageable pageable);

}
