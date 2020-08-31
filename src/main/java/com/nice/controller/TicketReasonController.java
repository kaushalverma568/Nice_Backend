/**
 *
 */
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

import com.nice.dto.TicketReasonDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TicketReasonMapper;
import com.nice.model.TicketReason;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.TicketReasonService;
import com.nice.validator.TicketReasonValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 19, 2020
 */
@RequestMapping(path = "/ticket/reason")
@RestController
public class TicketReasonController {

	/**
	 *
	 */
	private static final String TICKET_REASON_UPDATE_MESSAGE = "ticket.reason.update.message";

	private static final Logger LOGGER = LoggerFactory.getLogger(TicketReasonController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TicketReasonService ticketReasonService;

	@Autowired
	private TicketReasonMapper ticketReasonMapper;

	/**
	 * Validator - to apply/check any type of validation regarding category
	 */
	@Autowired
	private TicketReasonValidator ticketReasonValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(ticketReasonValidator);
	}

	/**
	 * add ticketReason
	 *
	 * @param  accessToken
	 * @param  ticketReasonDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping
	public ResponseEntity<Object> addTicketReason(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final TicketReasonDTO ticketReasonDTO, final BindingResult result) throws ValidationException {
		LOGGER.info("Inside add ticketReason method {}", ticketReasonDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ticketReason validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		ticketReasonService.addTicketReason(ticketReasonDTO);
		LOGGER.info("Outside add ticketReason method ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("ticket.reason.create.message", null)).create();
	}

	/**
	 * update ticketReason
	 *
	 * @param  accessToken
	 * @param  ticketReasonDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updateTicketReason(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final TicketReasonDTO ticketReasonDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update ticketReason method {}", ticketReasonDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("ticketReason validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		ticketReasonService.updateTicketReason(ticketReasonDTO);
		LOGGER.info("Outside update ticketReason method {}", ticketReasonDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(TICKET_REASON_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * get ticketReason
	 *
	 * @param  ticketReasonId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{ticketReasonId}")
	public ResponseEntity<Object> getTicketReason(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("ticketReasonId") final Long ticketReasonId) throws NotFoundException {
		LOGGER.info("Inside get ticketReason method {}", ticketReasonId);
		final TicketReasonDTO resultTicketReason = ticketReasonService.getTicketReason(ticketReasonId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("ticket.reason.detail.message", null)).setData(resultTicketReason).create();
	}

	/**
	 * get ticket reason type list
	 *
	 * @param  accessToken
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/type/list")
	public ResponseEntity<Object> getTicketReasonTypeList(@RequestHeader("Authorization") final String accessToken) throws NotFoundException {
		LOGGER.info("Inside get ticke tReason type list");
		final List<String> ticketReasonTypeList = ticketReasonService.getTicketReasonTypeList();
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.reason.list.message", null))
				.setData(ticketReasonTypeList).create();
	}

	/**
	 * To get list of ticketReasons
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getTicketReasonList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "type", required = false) final String type) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get ticketReason List ");
		final Page<TicketReason> resultTicketReasons = ticketReasonService.getTicketReasonList(pageNumber, pageSize, activeRecords, type);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.reason.list.message", null))
				.setData(ticketReasonMapper.toDtos(resultTicketReasons.getContent())).setHasNextPage(resultTicketReasons.hasNext())
				.setHasPreviousPage(resultTicketReasons.hasPrevious()).setTotalPages(resultTicketReasons.getTotalPages())
				.setPageNumber(resultTicketReasons.getNumber() + 1).setTotalCount(resultTicketReasons.getTotalElements()).create();
	}

	/**
	 * Change status of ticketReason (active/deActive)
	 *
	 * @param  accessToken
	 * @param  ticketReasonId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{ticketReasonId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("ticketReasonId") final Long ticketReasonId, @RequestParam("active") final Boolean active)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of ticketReason ofr id {} and status {}", ticketReasonId, active);
		ticketReasonService.changeStatus(ticketReasonId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(TICKET_REASON_UPDATE_MESSAGE, null))
				.create();
	}
}
