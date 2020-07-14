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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TicketMapper;
import com.nice.model.Ticket;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.TicketService;

/**
 * @author Kody Technolab PVT. LTD.
 * @date 31-Jan-2020
 */
@RequestMapping(path = "/ticket")
@RestController
public class TicketController {
	private static final Logger LOGGER = LoggerFactory.getLogger(TicketController.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TicketService ticketService;

	@Autowired
	private TicketMapper ticketMapper;

	/**
	 * Create Ticket
	 *
	 * @param TicketReason
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping()
	public ResponseEntity<Object> addTicket(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final TicketDTO ticketDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Ticket {}", ticketDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Ticket validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		ticketService.addTicket(ticketDTO);
		LOGGER.info("Outside add Ticket");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.create.message", null))
				.create();
	}

	/**
	 * Update Ticket
	 *
	 * @param ticketDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/{ticketId}")
	public ResponseEntity<Object> updateTicketStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("ticketId") final Long ticketId,
			@RequestParam("ticketStatus") final String ticketStatus, @RequestParam("comment") final String comment)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Ticket status ticketId:{} and ticketStatus:{} and comment:{}", ticketId, ticketStatus, comment);
		ticketService.updateTicketStatus(ticketId, ticketStatus, comment);
		LOGGER.info("Outside update Ticket");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.update.message", null))
				.create();
	}

	/**
	 * Get ticket detail based on ticketId
	 *
	 * @param ticketId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(path = "/{ticketId}")
	public ResponseEntity<Object> getTicket(@RequestHeader("Authorization") final String accessToken, @PathVariable("ticketId") final Long ticketId)
			throws NotFoundException {
		LOGGER.info("Inside get Ticket {}", ticketId);
		final TicketResponseDTO resultTicket = ticketService.getTicket(ticketId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.detail.message", null))
				.setData(resultTicket).create();
	}

	/**
	 * Get list of enquires based on parameters
	 *
	 * @param activeRecords
	 * @param pageNumber
	 * @param pageSize
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getTicketList(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "userType", required = false) final String userType, @PathVariable("pageNumber") final Integer pageNumber,
			@PathVariable("pageSize") final Integer pageSize) {
		final Page<Ticket> resultTicket = ticketService.getTicketList(userType, pageNumber, pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.list.message", null))
				.setData(ticketMapper.toDtos(resultTicket.getContent())).setHasNextPage(resultTicket.hasNext()).setHasPreviousPage(resultTicket.hasPrevious())
				.setTotalPages(resultTicket.getTotalPages()).setPageNumber(resultTicket.getNumber() + 1).setTotalCount(resultTicket.getTotalElements())
				.create();
	}

	/**
	 * Get ticket reason list
	 *
	 * @param accessToken
	 * @param type
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/reason/list/{type}")
	public ResponseEntity<Object> getTicketReasonList(@RequestHeader("Authorization") final String accessToken, @PathVariable("type") final String type)
			throws ValidationException {
		LOGGER.info("Inside get Ticket Reason list fot type:{}", type);
		final List<String> resultReasons = ticketService.getTicketReasonList(type);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.reason.list.message", null))
				.setData(resultReasons).create();
	}
}