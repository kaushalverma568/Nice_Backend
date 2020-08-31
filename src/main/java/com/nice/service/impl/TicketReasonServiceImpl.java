package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.TicketReasonType;
import com.nice.dto.TicketReasonDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TicketReasonMapper;
import com.nice.model.TicketReason;
import com.nice.repository.TicketReasonRepository;
import com.nice.service.TicketReasonService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 19, 2020
 */
@Service(value = "ticketReasonService")
@Transactional(rollbackFor = Throwable.class)
public class TicketReasonServiceImpl implements TicketReasonService {
	private static final Logger LOGGER = LoggerFactory.getLogger(TicketReasonServiceImpl.class);

	/**
	 *
	 */
	private static final String INVALID_TICKET_REASON_TYPE = "invalid.ticket.reason.type";

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TicketReasonRepository ticketReasonRepository;

	@Autowired
	private TicketReasonMapper ticketReasonMapper;

	@Override
	public void addTicketReason(final TicketReasonDTO ticketReasonDTO) throws ValidationException {
		if (TicketReasonType.getByValue(ticketReasonDTO.getType()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_TICKET_REASON_TYPE, null));
		}
		ticketReasonRepository.save(ticketReasonMapper.toEntity(ticketReasonDTO));
	}

	@Override
	public void updateTicketReason(final TicketReasonDTO ticketReasonDTO) throws NotFoundException, ValidationException {
		if (ticketReasonDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("ticket.reason.id.not.null", null));
		} else if (TicketReasonType.getByValue(ticketReasonDTO.getType()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_TICKET_REASON_TYPE, null));
		}
		ticketReasonRepository.save(ticketReasonMapper.toEntity(ticketReasonDTO));
	}

	@Override
	public TicketReasonDTO getTicketReason(final Long ticketReasonId) throws NotFoundException {
		return ticketReasonMapper.toDto(getTicketReasonDetails(ticketReasonId));
	}

	@Override
	public TicketReason getTicketReasonDetails(final Long ticketReasonId) throws NotFoundException {
		return ticketReasonRepository.findById(ticketReasonId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("ticket.reason.not.found", new Object[] { ticketReasonId })));
	}

	@Override
	public Page<TicketReason> getTicketReasonList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String type)
			throws NotFoundException, ValidationException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("reason"));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(type)) {
			if (TicketReasonType.getByValue(type) == null) {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_TICKET_REASON_TYPE, null));
			} else {
				if (activeRecords != null) {
					return ticketReasonRepository.findAllByActiveAndType(activeRecords, type, pageable);
				} else {
					return ticketReasonRepository.findAllByType(type, pageable);
				}
			}
		} else {
			if (activeRecords != null) {
				return ticketReasonRepository.findAllByActive(activeRecords, pageable);
			} else {
				return ticketReasonRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long ticketReasonId, final Boolean active) throws ValidationException, NotFoundException {
		final TicketReason existingTicketReason = getTicketReasonDetails(ticketReasonId);
		LOGGER.info("Existing ticketReason details {} ", existingTicketReason);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingTicketReason.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "ticket.reason.active" : "ticket.reason.deactive", null));
		} else {
			existingTicketReason.setActive(active);
			ticketReasonRepository.save(existingTicketReason);
		}
	}

	@Override
	public boolean isTicketReasonExists(final TicketReasonDTO ticketReasonDTO) {
		if (ticketReasonDTO.getId() != null) {
			/**
			 * At the time of update is ticketReason with same reason exist or not except its own id.
			 */
			return ticketReasonRepository
					.findByReasonIgnoreCaseAndTypeIgnoreCaseAndIdNot(ticketReasonDTO.getReason(), ticketReasonDTO.getType(), ticketReasonDTO.getId())
					.isPresent();
		} else {
			/**
			 * At the time of create is ticketReason with same reason exist or not
			 */
			return ticketReasonRepository.findByReasonIgnoreCaseAndTypeIgnoreCase(ticketReasonDTO.getReason(), ticketReasonDTO.getType()).isPresent();
		}
	}

	@Override
	public List<String> getTicketReasonTypeList() {
		List<String> ticketReasonTypeList = new ArrayList<>();
		for (final TicketReasonType TicketReasonType : TicketReasonType.values()) {
			ticketReasonTypeList.add(TicketReasonType.getStatusValue());
		}
		return ticketReasonTypeList;
	}
}
