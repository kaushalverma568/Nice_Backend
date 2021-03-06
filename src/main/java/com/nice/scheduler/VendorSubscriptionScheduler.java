package com.nice.scheduler;

import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.nice.exception.NotFoundException;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
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
		List<Long> vendorIds = vendorService.runVendorSubscriptionExpireScheduler(CommonUtility.convetUtilDatetoLocalDate(runDate));
		for (Long vendorId : vendorIds) {
			try {
				vendorService.sendEmailForChangeVendorStatus(vendorId);
			} catch (NotFoundException e) {
				LOGGER.info("not found exception while processing vendor subscription :{}", e.getMessage());
			}
		}
	}

	public void subscriptionExpireReminder(final Date runDate) {
		vendorService.runVendorSubscriptionExpireReminderScheduler(CommonUtility.convetUtilDatetoLocalDate(runDate));
	}
}
