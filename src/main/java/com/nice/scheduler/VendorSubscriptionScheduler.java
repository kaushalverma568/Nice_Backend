package com.nice.scheduler;

import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.nice.constant.Constant;
import com.nice.exception.NotFoundException;
import com.nice.service.SchedulerDetailsService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */

@Component
public class VendorSubscriptionScheduler {

	private static final Logger LOGGER = LoggerFactory.getLogger(VendorSubscriptionScheduler.class);

	@Autowired
	private VendorService vendorService;

	@Autowired
	private SchedulerDetailsService schedulerDetailsService;

	@Scheduled(cron = "0 30 0 * * ?")
	public void run() {
		LOGGER.info("vendor subscription expired -- Start: The time is now {}", new Date(System.currentTimeMillis()));
		expiredSubscription(new Date());
		updateRunDate(Constant.VENDOR_SUBSCRIPTION_EXPIRE);
		subscriptionExpireReminder(new Date());
		updateRunDate(Constant.VENDOR_SUBSCRIPTION_EXPIRE_REMINDER);
	}

	public void expiredSubscription(final Date runDate) {
		List<Long> vendorIds = vendorService.runVendorSubscriptionExpireScheduler(runDate);
		for (Long vendorId : vendorIds) {
			vendorService.sendEmailForChangeVendorStatus(vendorId);
		}
	}

	public void subscriptionExpireReminder(final Date runDate) {
		vendorService.runVendorSubscriptionExpireReminderScheduler(runDate);
	}

	private void updateRunDate(final String schedulerName) {
		try {
			schedulerDetailsService.updateSchedulerDate(schedulerName);
		} catch (NotFoundException e) {
			LOGGER.error("Error while updating date of scheduler {} ", e.getMessage());
		}
		LOGGER.info("{} -- End: The time is now {}", schedulerName, new Date(System.currentTimeMillis()));
	}
}
