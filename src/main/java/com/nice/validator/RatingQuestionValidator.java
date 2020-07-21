package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.RatingQuestionDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.RatingQuestionService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Component
public class RatingQuestionValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private RatingQuestionService ratingQuestionService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return RatingQuestionDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final RatingQuestionDTO ratingQuestionDTO = (RatingQuestionDTO) target;

		if (ratingQuestionService.isExists(ratingQuestionDTO)) {
			errors.rejectValue("question", "409", messageByLocaleService.getMessage("rating.question.not.unique", null));
		}
	}
}