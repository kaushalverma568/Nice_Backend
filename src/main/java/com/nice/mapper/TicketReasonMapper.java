package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.TicketReasonDTO;
import com.nice.model.TicketReason;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 19, 2020
 */
@Component
public class TicketReasonMapper {

	public TicketReasonDTO toDto(final TicketReason ticketReason) {
		TicketReasonDTO ticketReasonDTO = new TicketReasonDTO();
		BeanUtils.copyProperties(ticketReason, ticketReasonDTO);
		return ticketReasonDTO;
	}

	public TicketReason toEntity(final TicketReasonDTO ticketReasonDTO) {
		TicketReason ticketReason = new TicketReason();
		BeanUtils.copyProperties(ticketReasonDTO, ticketReason);
		return ticketReason;
	}

	public List<TicketReasonDTO> toDtos(final List<TicketReason> ticketReasons) {
		List<TicketReasonDTO> results = new ArrayList<>();
		for (TicketReason ticketReason : ticketReasons) {
			results.add(toDto(ticketReason));
		}
		return results;
	}
}
