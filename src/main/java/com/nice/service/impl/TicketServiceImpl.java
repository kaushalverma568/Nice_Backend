package com.nice.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.TicketReasonType;
import com.nice.constant.TicketStatusEnum;
import com.nice.constant.UserType;
import com.nice.dto.PushNotificationDTO;
import com.nice.dto.TicketDTO;
import com.nice.dto.TicketResponseDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TicketMapper;
import com.nice.model.Ticket;
import com.nice.model.TicketReason;
import com.nice.model.UserLogin;
import com.nice.repository.TicketReasonRepository;
import com.nice.repository.TicketRepository;
import com.nice.service.TicketReasonService;
import com.nice.service.TicketService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
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
	private TicketReasonService ticketReasonService;

	@Autowired
	private TicketMapper ticketMapper;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Override
	public Ticket addTicket(final TicketDTO ticketDTO) throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		LOGGER.info("Inside add ticket method for user {}", userLogin.getId());
		final Ticket ticket = ticketMapper.toEntity(ticketDTO);
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(ticketDTO.getTicketStatus())
				&& !ticketDTO.getTicketStatus().equals(TicketStatusEnum.PENDING.getStatusValue())) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_TICKET_STATUS, null));
		} else if (!(UserType.CUSTOMER.name().equals(userLogin.getEntityType()) || UserType.VENDOR.name().equals(userLogin.getEntityType())
				|| UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType()))) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.user.type.ticket", null));
		} else {
			TicketReason ticketReason = ticketReasonService.getTicketReasonDetails(ticketDTO.getTicketReasonId());
			/**
			 * set ticket status as pending for new ticket
			 */
			ticket.setEntityId(userLogin.getEntityId());
			ticket.setUserType(userLogin.getEntityType());
			ticket.setTicketStatus(TicketStatusEnum.PENDING.getStatusValue());
			ticket.setTicketReason(ticketReason);
			ticket.setActive(true);
			ticketRepository.save(ticket);
			return ticket;
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
			final Locale locale = LocaleContextHolder.getLocale();
			if (locale.getLanguage().equals("en")) {
				return ticketReasonRepository.findAllByType(type).stream().map(TicketReason::getReasonEnglish).collect(Collectors.toList());
			} else {
				return ticketReasonRepository.findAllByType(type).stream().map(TicketReason::getReasonArabic).collect(Collectors.toList());
			}
		}
	}

	@Override
	public Long getTicketCountBasedOnParams(final Long entityId, final String userType, final String name) {
		return ticketRepository.getTicketCountBasedOnParams(entityId, userType, name);
	}

	@Override
	public List<Ticket> getTicketListBasedOnParams(final Long entityId, final String userType, final String name, final Integer startIndex,
			final Integer pageSize) {
		return ticketRepository.getTicketListBasedOnParams(entityId, userType, name, startIndex, pageSize);
	}

	@Override
	public Page<Ticket> getTicketList(String userType, final Integer pageNumber, final Integer pageSize) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		LOGGER.info("Inside get ticket list for user {}", userLogin.getId());
		Long entityId = null;
		/**
		 * if login user is delivery boy , vendor or customer then get ticket list of
		 * that user only
		 */
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType()) || UserType.VENDOR.name().equals(userLogin.getEntityType())
				|| UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			userType = userLogin.getEntityType();
			entityId = userLogin.getEntityId();
		}

		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (entityId != null) {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userType)) {
				return ticketRepository.findAllByEntityIdAndUserType(entityId, userType, pageable);
			} else {
				return ticketRepository.findAllByEntityId(entityId, pageable);
			}
		} else {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userType)) {
				return ticketRepository.findAllByUserType(userType, pageable);
			} else {
				return ticketRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void exportList(final String userType, final String name, final HttpServletResponse httpServletResponse)
			throws NotFoundException, FileNotFoundException {
		List<Ticket> ticketList = ticketRepository.getTicketListBasedOnParams(null, userType, name, null, null);
		List<TicketResponseDTO> ticketExportList = new ArrayList<>();

		for (Ticket ticket : ticketList) {
			ticketExportList.add(ticketMapper.toDto(ticket));
		}
		final Object[] ticketHeaderField = new Object[] { "Name", "Email", "Phone Number", "TicketStatus", "Description", "Comment", "Created Date" };
		final Object[] ticketDataField = new Object[] { "name", "email", "phoneNumber", "ticketStatus", "description", "comment", "createdAt" };
		try {
			exportCSV.writeCSVFile(ticketExportList, ticketDataField, ticketHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}

	}

	@Override
	public void sendPushNotificationForNewTicket(final Long ticketId) throws NotFoundException {
		PushNotificationDTO pushNotificationDTO = new PushNotificationDTO();
		pushNotificationDTO.setTicketId(ticketId);
		pushNotificationDTO.setType(NotificationQueueConstants.NEW_TICKET_PUSH_NOTIFICATION);
		jmsQueuerService.sendPushNotification(NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE, pushNotificationDTO);
	}

}
