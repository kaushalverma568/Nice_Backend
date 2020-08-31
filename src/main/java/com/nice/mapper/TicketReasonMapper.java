package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.TicketReasonDTO;
import com.nice.model.TicketReason;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : Aug 19, 2020
 */
@Component
public class TicketReasonMapper {

	public TicketReasonDTO toDto(final TicketReason ticketReason) {
		Locale locale = LocaleContextHolder.getLocale();
		TicketReasonDTO ticketReasonDTO = new TicketReasonDTO();
		BeanUtils.copyProperties(ticketReason, ticketReasonDTO);
		if (locale.getLanguage().equals("en")) {
			ticketReasonDTO.setReason(ticketReason.getReasonEnglish());
		} else {
			ticketReasonDTO.setReason(ticketReason.getReasonArabic());
		}
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
