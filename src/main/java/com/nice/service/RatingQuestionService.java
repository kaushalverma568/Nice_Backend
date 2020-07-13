package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.RatingQuestionDTO;
import com.nice.model.RatingQuestion;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

public interface RatingQuestionService {

	/**
	 * 
	 * @param ratingQuestionDto
	 * @return
	 */
	public boolean isExists(RatingQuestionDTO ratingQuestionDto) ;

	/**
	 * 
	 * @param ratingQuestionDto
	 * @return
	 * @throws NotFoundException
	 */
	public RatingQuestionDTO addRatingQuestion( RatingQuestionDTO ratingQuestionDto) throws NotFoundException ;

	/**
	 * 
	 * @param ratingQuestionDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	public RatingQuestionDTO updateRatingQuestion( RatingQuestionDTO ratingQuestionDTO) throws NotFoundException, ValidationException;

	/**
	 * 
	 * @param ratingQuestionId
	 * @return
	 * @throws NotFoundException
	 */
	public RatingQuestionDTO getRatingQuestion(Long ratingQuestionId) throws NotFoundException;

	/**
	 * 
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyWord
	 * @return
	 */
	public Page<RatingQuestion> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * 
	 * @param ratingQuestionId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void changeStatus(Long ratingQuestionId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param ratingQuestionId
	 * @return
	 * @throws NotFoundException
	 */
	RatingQuestion getRatingQuestionDetail(Long ratingQuestionId) throws NotFoundException;
	
}
