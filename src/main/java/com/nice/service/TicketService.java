package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Ticket;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 09-07-2020
 */
public interface TicketService {

	/**
	 * create ticket
	 *
	 * @param  ticketDTO
	 * @param  userId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addTicket(TicketDTO ticketDTO) throws ValidationException, NotFoundException;

	/**
	 * get ticket by ticketId
	 *
	 * @param  ticketId
	 * @return
	 * @throws NotFoundException
	 */
	TicketResponseDTO getTicket(Long ticketId) throws NotFoundException;

	/**
	 * get ticket detail by ticketId
	 *
	 * @param  ticketId
	 * @return
	 * @throws NotFoundException
	 */
	Ticket getTicketDetail(Long ticketId) throws NotFoundException;

	/**
	 * get page of ticket by parameter
	 *
	 * @param  userType
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 */
	Page<Ticket> getTicketList(String userType, Integer pageNumber, Integer pageSize);

	/**
	 * update ticket status
	 *
	 * @param  ticketId
	 * @param  ticketStatus
	 * @param  comment
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateTicketStatus(Long ticketId, String ticketStatus, String comment) throws ValidationException, NotFoundException;

	/**
	 * get ticket reason list based on type
	 *
	 * @param  type
	 * @return
	 * @throws ValidationException
	 */
	List<String> getTicketReasonList(String type) throws ValidationException;

	/**
	 * get ticket count based on parameters
	 *
	 * @param  entityId
	 * @param  userType
	 * @param  name
	 * @return
	 */
	Long getTicketCountBasedOnParams(Long entityId, String userType, String name);

	/**
	 * get ticket list based on parameters
	 *
	 * @param  entityId
	 * @param  userType
	 * @param  name
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<Ticket> getTicketListBasedOnParams(Long entityId, String userType, String name, Integer startIndex, Integer pageSize);

}
