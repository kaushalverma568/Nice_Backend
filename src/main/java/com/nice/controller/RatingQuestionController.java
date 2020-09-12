package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.RatingQuestionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.RatingQuestionMapper;
import com.nice.model.RatingQuestion;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.RatingQuestionService;
import com.nice.validator.RatingQuestionValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */

@RequestMapping(path = "/ratingQuestion")
@RestController
public class RatingQuestionController {

	private static final Logger LOGGER = LoggerFactory.getLogger(RatingQuestionController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private RatingQuestionService ratingQuestionService;

	@Autowired
	private RatingQuestionMapper ratingQuestionMapper;

	/**
	 * validator - to apply/check any type of validation regarding sections
	 */

	@Autowired
	private RatingQuestionValidator ratingQuestionValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(ratingQuestionValidator);
	}

	@PostMapping
	@PreAuthorize("hasPermission('Rating Question','CAN_ADD')")
	public ResponseEntity<Object> addRatingQuestion(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final RatingQuestionDTO ratingQuestionDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add RatingQuestion {}", ratingQuestionDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("RatingQuestion validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		RatingQuestionDTO resultRatingQuestion = ratingQuestionService.addRatingQuestion(ratingQuestionDTO);
		LOGGER.info("Outside add RatingQuestion {}", resultRatingQuestion);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("rating.question.create.message", null)).setData(resultRatingQuestion).create();
	}

	@PutMapping
	@PreAuthorize("hasPermission('Rating Question','CAN_EDIT')")
	public ResponseEntity<Object> updateRatingQuestion(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final RatingQuestionDTO ratingQuestionDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update RatingQuestion {}", ratingQuestionDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("RatingQuestion validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		RatingQuestionDTO resultRatingQuestion = ratingQuestionService.updateRatingQuestion(ratingQuestionDTO);
		LOGGER.info("Outside update RatingQuestion {}", resultRatingQuestion);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("rating.question.update.message", null)).setData(resultRatingQuestion).create();
	}

	@GetMapping(value = "/{ratingQuestionId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("ratingQuestionId") final Long ratingQuestionId) throws NotFoundException {
		RatingQuestionDTO resultRatingQuestion = ratingQuestionService.getRatingQuestion(ratingQuestionId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("rating.question.detail.message", null)).setData(resultRatingQuestion).create();
	}

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "type", required = false) final String type) {
		final Page<RatingQuestion> resultRatingQuestion = ratingQuestionService.getList(pageNumber, pageSize, type);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("rating.question.list.message", null))
				.setData(ratingQuestionMapper.toDtos(resultRatingQuestion.getContent())).setHasNextPage(resultRatingQuestion.hasNext())
				.setHasPreviousPage(resultRatingQuestion.hasPrevious()).setTotalPages(resultRatingQuestion.getTotalPages())
				.setPageNumber(resultRatingQuestion.getNumber() + 1).setTotalCount(resultRatingQuestion.getTotalElements()).create();
	}

	@PutMapping("/status/{ratingQuestionId}")
	@PreAuthorize("hasPermission('Rating Question','CAN_DELETE')")
	public ResponseEntity<Object> updateStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("ratingQuestionId") final Long ratingQuestionId, @RequestParam final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of RatingQuestion of id {} and status {}", ratingQuestionId, active);
		ratingQuestionService.changeStatus(ratingQuestionId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("rating.question.update.message", null)).create();
	}
}