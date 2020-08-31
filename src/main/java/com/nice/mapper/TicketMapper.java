package com.nice.mapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.TicketStatusEnum;
import com.nice.constant.UserType;
import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.Ticket;
import com.nice.model.Vendor;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.VendorService;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 09-07-2020
 */
@Component
public class TicketMapper {

	@Autowired
	private CustomerService customerService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private VendorService vendorService;

	public TicketResponseDTO toDto(final Ticket ticket) throws NotFoundException {
		final Locale locale = LocaleContextHolder.getLocale();
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
		if (UserType.CUSTOMER.name().equals(ticket.getUserType())) {
			Customer customer = customerService.getCustomerDetails(ticket.getEntityId());
			ticketResponseDTO.setEmail(customer.getEmail());
			ticketResponseDTO.setName(customer.getFirstName() + " " + customer.getLastName());
			ticketResponseDTO.setPhoneNumber(customer.getPhoneNumber());

		} else if (UserType.VENDOR.name().equals(ticket.getUserType())) {
			Vendor vendor = vendorService.getVendorDetail(ticket.getEntityId());
			ticketResponseDTO.setEmail(vendor.getEmail());
			ticketResponseDTO.setName(vendor.getFirstName() + " " + vendor.getLastName());
			ticketResponseDTO.setPhoneNumber(vendor.getPhoneNumber());

		} else if (UserType.DELIVERY_BOY.name().equals(ticket.getUserType())) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(ticket.getEntityId());
			ticketResponseDTO.setEmail(deliveryBoy.getEmail());
			ticketResponseDTO.setNameEnglish(deliveryBoy.getFirstNameEnglish() + " " + deliveryBoy.getLastNameEnglish());
			ticketResponseDTO.setNameArabic(deliveryBoy.getFirstNameArabic() + " " + deliveryBoy.getLastNameArabic());
			if (locale.getLanguage().equals("en")) {
				ticketResponseDTO.setName(deliveryBoy.getFirstNameEnglish() + " " + deliveryBoy.getLastNameEnglish());
			} else {
				ticketResponseDTO.setName(deliveryBoy.getFirstNameArabic() + " " + deliveryBoy.getLastNameArabic());
			}
			ticketResponseDTO.setPhoneNumber(deliveryBoy.getPhoneNumber());
		}
		return ticketResponseDTO;
	}

	public Ticket toEntity(final TicketDTO ticketDTO) {
		Ticket ticket = new Ticket();
		BeanUtils.copyProperties(ticketDTO, ticket);
		return ticket;
	}

	public List<TicketResponseDTO> toDtos(final List<Ticket> ticketList) throws NotFoundException {
		List<TicketResponseDTO> results = new ArrayList<>();
		for (Ticket ticket : ticketList) {
			results.add(toDto(ticket));
		}
		return results;
	}
}
