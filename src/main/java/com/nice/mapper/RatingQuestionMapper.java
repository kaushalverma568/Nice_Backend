package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.RatingQuestionDTO;
import com.nice.model.RatingQuestion;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public class RatingQuestionMapper {
	
	public RatingQuestionDTO toDto(final RatingQuestion ratingQuestion) {
		RatingQuestionDTO ratingQuestionResponseDTO = new RatingQuestionDTO();
		BeanUtils.copyProperties(ratingQuestion, ratingQuestionResponseDTO);
		return ratingQuestionResponseDTO;
	}

	public RatingQuestion toEntity(final RatingQuestionDTO ratingQuestionDTO) {
		RatingQuestion ratingQuestion = new RatingQuestion();
		BeanUtils.copyProperties(ratingQuestionDTO, ratingQuestion);
		return ratingQuestion;
	}

	public List<RatingQuestionDTO> toDtos(final List<RatingQuestion> ratingQuestionList) {
		List<RatingQuestionDTO> results = new ArrayList<>();
		for (RatingQuestion RatingQuestion : ratingQuestionList) {
			results.add(toDto(RatingQuestion));
		}
		return results;
	}
}
