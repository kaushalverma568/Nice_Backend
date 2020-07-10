package com.nice.mapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.constant.TicketStatusEnum;
import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.model.Ticket;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 09-07-2020
 */
@Component
public class TicketMapper {

	public TicketResponseDTO toDto(final Ticket ticket) {
		TicketResponseDTO ticketResponseDTO = new TicketResponseDTO();
		BeanUtils.copyProperties(ticket, ticketResponseDTO);

		if (ticket.getTicketStatus() != null) {
			final TicketStatusEnum ticketOldStatus = TicketStatusEnum.valueOf(TicketStatusEnum.getByValue(ticketResponseDTO.getTicketStatus()).name());
			if (ticketOldStatus.nextStatus() == null) {
				ticketResponseDTO.setNextStatus(Collections.emptyList());
			} else {
				final List<TicketStatusEnum> ticketStatusList = Arrays.asList(ticketOldStatus.nextStatus());
				ticketResponseDTO.setNextStatus(ticketStatusList.stream().map(TicketStatusEnum::getStatusValue).collect(Collectors.toList()));
			}
		}
		return ticketResponseDTO;
	}

	public Ticket toEntity(final TicketDTO ticketDTO) {
		Ticket ticket = new Ticket();
		BeanUtils.copyProperties(ticketDTO, ticket);
		return ticket;
	}

	public List<TicketResponseDTO> toDtos(final List<Ticket> ticketList) {
		List<TicketResponseDTO> results = new ArrayList<>();
		for (Ticket ticket : ticketList) {
			results.add(toDto(ticket));
		}
		return results;
	}
}
