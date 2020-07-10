package com.nice.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.TicketReasonType;
import com.nice.constant.TicketStatusEnum;
import com.nice.constant.UserType;
import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TicketMapper;
import com.nice.model.Ticket;
import com.nice.model.TicketReason;
import com.nice.model.UserLogin;
import com.nice.repository.TicketReasonRepository;
import com.nice.repository.TicketRepository;
import com.nice.service.TicketService;
import com.nice.util.CommonUtility;

/**
 * @author Kody Technolab PVT. LTD.
 * @date 31-Jan-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("ticketService")
public class TicketServiceImpl implements TicketService {

	/**
	 *
	 */
	private static final String INVALID_TICKET_STATUS = "invalid.ticket.status";

	private static final Logger LOGGER = LoggerFactory.getLogger(TicketServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TicketRepository ticketRepository;

	@Autowired
	private TicketReasonRepository ticketReasonRepository;

	@Autowired
	private TicketMapper ticketMapper;

	@Override
	public void addTicket(final TicketDTO ticketDTO) throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		LOGGER.info("Inside add ticket method for user {}", userLogin.getId());
		final Ticket ticket = ticketMapper.toEntity(ticketDTO);
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(ticketDTO.getTicketStatus())
				&& !ticketDTO.getTicketStatus().equals(TicketStatusEnum.PENDING.getStatusValue())) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_TICKET_STATUS, null));
		} else if (!(UserType.CUSTOMER.name().equalsIgnoreCase(userLogin.getEntityType()) || UserType.VENDOR.name().equalsIgnoreCase(userLogin.getEntityType())
				|| UserType.DELIVERY_BOY.name().equalsIgnoreCase(userLogin.getEntityType()))) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.user.type.ticket", null));
		} else if (!ticketReasonRepository.findByReason(ticket.getTicketReason()).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.ticket.reason", null));
		} else {

			/**
			 * set ticket status as pending for new ticket
			 */
			ticket.setEmail(userLogin.getEmail());
			ticket.setUserType(userLogin.getEntityType());
			ticket.setTicketStatus(TicketStatusEnum.PENDING.getStatusValue());
			ticket.setActive(true);
			ticketRepository.save(ticket);
		}
	}

	@Override
	public void updateTicketStatus(final Long ticketId, final String ticketStatus, final String comment) throws ValidationException, NotFoundException {
		if (TicketStatusEnum.getByValue(ticketStatus) == null) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_TICKET_STATUS, null));
		} else {
			final Ticket ticket = getTicketDetail(ticketId);
			/**
			 * new ticket status
			 */
			final TicketStatusEnum ticketNewStatus = TicketStatusEnum.valueOf(TicketStatusEnum.getByValue(ticketStatus).name());
			/**
			 * old ticket status
			 */
			final TicketStatusEnum ticketOldStatus = TicketStatusEnum.valueOf(TicketStatusEnum.getByValue(ticket.getTicketStatus()).name());
			/**
			 * check ticket status can change from old status to new status
			 */
			if (ticketOldStatus.nextStatus() == null) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.status.message", new Object[] { ticketNewStatus.getStatusValue() }));
			}
			final List<TicketStatusEnum> ticketStatusList = Arrays.asList(ticketOldStatus.nextStatus());
			/**
			 * Throws exception if next status requested is invalid
			 */
			if (!ticketStatusList.contains(ticketNewStatus)) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.status.message", new Object[] { ticketNewStatus.getStatusValue() }));
			} else {
				ticket.setComment(comment);
				ticket.setTicketStatus(ticketStatus);
				ticketRepository.save(ticket);
			}
		}
	}

	@Override
	public Ticket getTicketDetail(final Long ticketId) throws NotFoundException {
		return ticketRepository.findById(ticketId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("ticket.not.found", new Object[] { ticketId })));
	}

	@Override
	public TicketResponseDTO getTicket(final Long ticketId) throws NotFoundException {
		return ticketMapper.toDto(getTicketDetail(ticketId));
	}

	@Override
	public List<String> getTicketReasonList(final String type) throws ValidationException {
		if (TicketReasonType.getByValue(type) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.ticket.reason.type", null));
		} else {
			return ticketReasonRepository.findAllByType(type).stream().map(TicketReason::getReason).collect(Collectors.toList());
		}
	}

	@Override
	public Page<Ticket> getTicketList(String userType, final Integer pageNumber, final Integer pageSize) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		LOGGER.info("Inside get ticket list for user {}", userLogin.getId());
		String email = null;
		/**
		 * if login user is delivery boy , vendor or customer then get ticket list of
		 * that user only
		 */
		if (UserType.CUSTOMER.name().equalsIgnoreCase(userLogin.getEntityType()) || UserType.VENDOR.name().equalsIgnoreCase(userLogin.getEntityType())
				|| UserType.DELIVERY_BOY.name().equalsIgnoreCase(userLogin.getEntityType())) {
			userType = userLogin.getEntityType();
			email = userLogin.getEmail();
		}

		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(email)) {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userType)) {
				return ticketRepository.findAllByEmailAndUserType(email, userType, pageable);
			} else {
				return ticketRepository.findAllByEmail(email, pageable);
			}
		} else {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userType)) {
				return ticketRepository.findAllByUserType(userType, pageable);
			} else {
				return ticketRepository.findAll(pageable);
			}
		}
	}
}
