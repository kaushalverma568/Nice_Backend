package com.nice.scheduler;

import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

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

	@Scheduled(cron = "0 30 0 * * ?")
	public void run() {
		LOGGER.info("vendor subscription expired -- Start: The time is now {}", new Date(System.currentTimeMillis()));
		expiredSubscription(new Date());
		subscriptionExpireReminder(new Date());
	}

	public void expiredSubscription(final Date runDate) {
		vendorService.runVendorSubscriptionExpireScheduler(runDate);
	}

	public void subscriptionExpireReminder(final Date runDate) {
		vendorService.runVendorSubscriptionExpireReminderScheduler(runDate);
	}

}
