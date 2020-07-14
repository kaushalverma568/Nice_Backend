package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.RatingQuestion;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface RatingQuestionRepository extends JpaRepository<RatingQuestion, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<RatingQuestion> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * 
	 * @param question
	 * @param id
	 * @return
	 */
	Optional<RatingQuestion> findByQuestionIgnoreCaseAndIdNot(String question, Long id);

	/**
	 * 
	 * @param question
	 * @return
	 */
	Optional<RatingQuestion> findByQuestionIgnoreCase(String question);

}
