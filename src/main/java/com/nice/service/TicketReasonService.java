package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.TicketReasonDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.TicketReason;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 19, 2020
 */
public interface TicketReasonService {
	/**
	 * Add ticketReason
	 *
	 * @param  ticketReasonDTO
	 * @return
	 * @throws ValidationException
	 */
	void addTicketReason(TicketReasonDTO ticketReasonDTO) throws ValidationException;

	/**
	 * Update ticketReason
	 *
	 * @param  ticketReason
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateTicketReason(TicketReasonDTO ticketReasonDTO) throws NotFoundException, ValidationException;

	/**
	 * Get details of ticketReason
	 *
	 * @param  ticketReasonId
	 * @return
	 * @throws NotFoundException
	 */
	TicketReasonDTO getTicketReason(Long ticketReasonId) throws NotFoundException;

	/**
	 * to check ticketReason duplication and returning Boolean value.
	 *
	 * @param  ticketReason
	 * @return
	 * @throws ValidationException
	 */
	boolean isTicketReasonExists(TicketReasonDTO ticketReasonDTO);

	/**
	 * Get TicketReason details based on Id : Specially for internally calls
	 *
	 * @param  ticketReasonId
	 * @return
	 * @throws NotFoundException
	 */
	TicketReason getTicketReasonDetails(Long ticketReasonId) throws NotFoundException;

	/**
	 * Change status of ticketReason (active/deActive)
	 *
	 * @param  ticketReasonId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long ticketReasonId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * Get List of ticketReason based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  type
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<TicketReason> getTicketReasonList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String type)
			throws NotFoundException, ValidationException;

	/**
	 * get ticket reason type list
	 *
	 * @return
	 */
	List<String> getTicketReasonTypeList();

}
