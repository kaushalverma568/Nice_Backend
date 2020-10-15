package com.nice.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.SchedulerDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SchedulerMapper;
import com.nice.model.SchedulerDetails;
import com.nice.repository.SchedulerDetailsRepository;
import com.nice.service.SchedulerDetailsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("schedulerDetailsService")
public class SchedulerDetailsServiceImpl implements SchedulerDetailsService {

	private static final Logger LOGGER = LoggerFactory.getLogger(SchedulerDetailsServiceImpl.class);
	@Autowired
	private SchedulerDetailsRepository schedulerDetailsRepository;

	@Autowired
	private SchedulerMapper schedulerMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void updateSchedulerDate(final String name, final Date date) throws NotFoundException {
		LOGGER.info("Updating scheduler for {}", date);
		Optional<SchedulerDetails> optionalSchedulerDetails = schedulerDetailsRepository.findByName(name);
		if (!optionalSchedulerDetails.isPresent()) {
			LOGGER.error("No Scheduler present with name : {}", name);
			throw new NotFoundException(messageByLocaleService.getMessage("scheduler.not.found", new Object[] { name }));
		}
		SchedulerDetails schedulerDetails = optionalSchedulerDetails.get();
		schedulerDetails.setUpdatedAt(date);
		schedulerDetailsRepository.save(schedulerDetails);
	}

	@Override
	public SchedulerDetails getLastSchedulerRunDate(final String name) throws NotFoundException {

		Optional<SchedulerDetails> optionalSchedulerDetails = schedulerDetailsRepository.findByName(name);
		if (!optionalSchedulerDetails.isPresent()) {
			LOGGER.error("No Scheduler present with name : {}", name);
			throw new NotFoundException(messageByLocaleService.getMessage("scheduler.not.found", new Object[] { name }));
		}
		return optionalSchedulerDetails.get();
	}

	@Override
	public List<SchedulerDetailDTO> getSchedulerList() {
		LOGGER.info("Inside getList method of scheduler");
		return schedulerMapper.toDtos(schedulerDetailsRepository.findAll());
	}

}
