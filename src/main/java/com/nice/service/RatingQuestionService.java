package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.RatingQuestionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.RatingQuestion;

public interface RatingQuestionService {

	/**
	 * @param  ratingQuestionDto
	 * @return
	 */
	boolean isExistsEnglish(RatingQuestionDTO ratingQuestionDto);

	/**
	 * @param  ratingQuestionDto
	 * @return
	 */
	boolean isExistsArabic(RatingQuestionDTO ratingQuestionDto);

	/**
	 * @param  ratingQuestionDto
	 * @return
	 * @throws NotFoundException
	 */
	RatingQuestionDTO addRatingQuestion(RatingQuestionDTO ratingQuestionDto) throws NotFoundException;

	/**
	 * @param  ratingQuestionDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	RatingQuestionDTO updateRatingQuestion(RatingQuestionDTO ratingQuestionDTO) throws NotFoundException, ValidationException;

	/**
	 * @param  ratingQuestionId
	 * @return
	 * @throws NotFoundException
	 */
	RatingQuestionDTO getRatingQuestion(Long ratingQuestionId) throws NotFoundException;

	/**
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @return
	 */
	Page<RatingQuestion> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * @param  ratingQuestionId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long ratingQuestionId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param  ratingQuestionId
	 * @return
	 * @throws NotFoundException
	 */
	RatingQuestion getRatingQuestionDetail(Long ratingQuestionId) throws NotFoundException;

}
