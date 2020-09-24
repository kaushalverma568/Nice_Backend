package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
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

import com.nice.dto.PaginationUtilDto;
import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TicketMapper;
import com.nice.model.Ticket;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.TicketService;
import com.nice.util.PaginationUtil;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : Aug 7, 2020
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
	@PreAuthorize("hasPermission('Ticket','CAN_ADD')")
	public ResponseEntity<Object> addTicket(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final TicketDTO ticketDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Ticket {}", ticketDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Ticket validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Ticket ticket = ticketService.addTicket(ticketDTO);
		LOGGER.info("Outside add Ticket");
		/**
		 * send push notification to admin
		 */
		ticketService.sendPushNotificationForNewTicket(ticket.getId());
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
	@PreAuthorize("hasPermission('Ticket','CAN_EDIT')")
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
	@PreAuthorize("hasPermission('Ticket','CAN_VIEW')")
	public ResponseEntity<Object> getTicket(@RequestHeader("Authorization") final String accessToken, @PathVariable("ticketId") final Long ticketId)
			throws NotFoundException {
		LOGGER.info("Inside get Ticket {}", ticketId);
		final TicketResponseDTO resultTicket = ticketService.getTicket(ticketId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.detail.message", null))
				.setData(resultTicket).create();
	}

	/**
	 * Get ticket list based on parameters for user
	 *
	 * @param accessToken
	 * @param userType
	 * @param name
	 * @param entityId
	 * @param pageNumber
	 * @param pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/list/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Ticket','CAN_VIEW')")
	public ResponseEntity<Object> getTicketListBasedOnParams(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "userType", required = false) final String userType, @RequestParam(name = "name", required = false) final String name,
			@RequestParam(name = "entityId", required = false) final Long entityId, @PathVariable("pageNumber") final Integer pageNumber,
			@PathVariable("pageSize") final Integer pageSize) throws ValidationException, NotFoundException {
		LOGGER.info("Inside get ticket List");
		Long totalCount = ticketService.getTicketCountBasedOnParams(entityId, userType, name);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);

		final List<Ticket> ticketList = ticketService.getTicketListBasedOnParams(entityId, userType, name, paginationUtilDto.getStartIndex(), pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.list.message", null))
				.setData(ticketMapper.toDtos(ticketList)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();
	}

	/**
	 * Export ticket
	 *
	 * @param accessToken
	 * @param httpServletResponse
	 * @param name
	 * @param userType
	 * @return
	 * @throws FileNotFoundException
	 * @throws NotFoundException
	 */
	@GetMapping("/export/list")
	@PreAuthorize("hasPermission('Ticket','CAN_VIEW')")
	public ResponseEntity<Object> exportList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse,
			@RequestParam(name = "name", required = false) final String name, @RequestParam(name = "userType", required = false) final String userType)
			throws FileNotFoundException, NotFoundException {
		ticketService.exportList(userType, name, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("ticket.list.message", null))
				.create();
	}
}
