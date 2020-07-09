package com.nice.controller;

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.constant.Constant;
import com.nice.dto.SchedulerDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.SchedulerDetails;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.SchedulerDetailsService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@RestController
@RequestMapping("/scheduler")
public class SchedulerController {

	private static final Logger LOGGER = LoggerFactory.getLogger(SchedulerController.class);

	@Autowired
	private SchedulerDetailsService schedulerDetailsService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@GetMapping
	public ResponseEntity<Object> getSchedulerList(@RequestHeader("Authorization") final String accessToken) {
		LOGGER.info("Inside get Scheduler list");
		List<SchedulerDetailDTO> schedulerDetailsList = schedulerDetailsService.getSchedulerList();
		return new GenericResponseHandlers.Builder().setData(schedulerDetailsList).setMessage(messageByLocaleService.getMessage("scheduler.list.message", null))
				.setStatus(HttpStatus.OK).create();
	}

	@PutMapping("/run/{name}")
	public ResponseEntity<Object> runScheduler(@RequestHeader("Authorization") final String accessToken, @PathVariable final String name)
			throws ValidationException, NotFoundException {
		final LocalDate runDate = LocalDate.now();
		LOGGER.info("Inside run Scheduler of discount for date :{}", runDate);
		if (!(Constant.VENDOR_SUBSCRIPTION_EXPIRE.equals(name) || Constant.VENDOR_SUBSCRIPTION_EXPIRE_REMINDER.equals(name))) {
			throw new NotFoundException(messageByLocaleService.getMessage("scheduler.not.found", new Object[] { name }));
		}
		SchedulerDetails schedulerDetails = schedulerDetailsService.getLastSchedulerRunDate(name);
		/**
		 * if scheduler is already run after this date then throw exception
		 */
		if (CommonUtility.convetUtilDatetoLocalDate(schedulerDetails.getUpdatedAt()).compareTo(runDate) >= 0) {
			throw new ValidationException(messageByLocaleService.getMessage("run.date.invalid", new Object[] { name, schedulerDetails.getUpdatedAt() }));
		}
		if (Constant.VENDOR_SUBSCRIPTION_EXPIRE.equals(name)) {
			vendorService.runVendorSubscriptionExpireScheduler(new Date());
		} else if (Constant.VENDOR_SUBSCRIPTION_EXPIRE_REMINDER.equals(name)) {
			vendorService.runVendorSubscriptionExpireReminderScheduler(new Date());
		}
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("scheduler.run.successfully", new Object[] { name }))
				.setStatus(HttpStatus.OK).create();
	}
}
