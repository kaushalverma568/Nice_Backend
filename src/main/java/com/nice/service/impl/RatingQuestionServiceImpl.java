package com.nice.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.RatingQuestionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.RatingQuestionMapper;
import com.nice.model.RatingQuestion;
import com.nice.repository.RatingQuestionRepository;
import com.nice.service.RatingQuestionService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class RatingQuestionServiceImpl implements RatingQuestionService {

	private static final Logger LOGGER = LoggerFactory.getLogger(RatingQuestionServiceImpl.class);

	@Autowired
	private RatingQuestionRepository ratingQuestionRepository;

	@Autowired
	private RatingQuestionMapper ratingQuestionMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public RatingQuestionDTO addRatingQuestion(final RatingQuestionDTO ratingQuestionDTO) throws NotFoundException {
		return ratingQuestionMapper.toDto(ratingQuestionRepository.save(ratingQuestionMapper.toEntity(ratingQuestionDTO)));
	}

	@Override
	public RatingQuestionDTO updateRatingQuestion(final RatingQuestionDTO ratingQuestionDTO) throws NotFoundException, ValidationException {
		if (ratingQuestionDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("rating.question.id.not.null", null));
		}
		return ratingQuestionMapper.toDto(ratingQuestionRepository.save(ratingQuestionMapper.toEntity(ratingQuestionDTO)));
	}

	@Override
	public RatingQuestionDTO getRatingQuestion(final Long ratingQuestionId) throws NotFoundException {
		return ratingQuestionMapper.toDto(getRatingQuestionDetail(ratingQuestionId));
	}

	@Override
	public void changeStatus(final Long ratingQuestionId, final Boolean active) throws ValidationException, NotFoundException {
		RatingQuestion existingRatingQuestion = getRatingQuestionDetail(ratingQuestionId);
		LOGGER.info("Existing  RatingQuestion details {} ", existingRatingQuestion);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingRatingQuestion.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "rating.question.active" : "rating.question.deactive", null));
		} else {
			existingRatingQuestion.setActive(active);
			ratingQuestionRepository.save(existingRatingQuestion);
		}
	}

	@Override
	public Page<RatingQuestion> getList(final Integer pageNumber, final Integer pageSize, final String type) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (type != null) {
			return ratingQuestionRepository.findAllByType(type, pageable);

		} else {
			return ratingQuestionRepository.findAll(pageable);
		}
	}

	@Override
	public boolean isExistsEnglish(final RatingQuestionDTO ratingQuestionDTO) {
		if (ratingQuestionDTO.getId() != null) {
			return !(ratingQuestionRepository.findByQuestionEnglishIgnoreCaseAndIdNot(ratingQuestionDTO.getQuestionEnglish(), ratingQuestionDTO.getId())
					.isPresent());

		} else {
			return !(ratingQuestionRepository.findByQuestionEnglishIgnoreCase(ratingQuestionDTO.getQuestionEnglish()).isPresent());
		}
	}

	@Override
	public boolean isExistsArabic(final RatingQuestionDTO ratingQuestionDTO) {
		if (ratingQuestionDTO.getId() != null) {
			return !(ratingQuestionRepository.findByQuestionArabicIgnoreCaseAndIdNot(ratingQuestionDTO.getQuestionArabic(), ratingQuestionDTO.getId())
					.isPresent());

		} else {
			return !(ratingQuestionRepository.findByQuestionArabicIgnoreCase(ratingQuestionDTO.getQuestionArabic()).isPresent());
		}
	}

	@Override
	public RatingQuestion getRatingQuestionDetail(final Long RatingQuestionId) throws NotFoundException {
		return ratingQuestionRepository.findById(RatingQuestionId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("rating.question.not.found", new Object[] { RatingQuestionId })));
	}

}
