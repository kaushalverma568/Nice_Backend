package com.nice.scheduler;

import java.time.LocalDate;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.nice.exception.NotFoundException;
import com.nice.service.DiscountService;
import com.nice.service.OrderRatingService;
import com.nice.service.StockDetailsService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Mar-2020
 * @description :
 */
@Component
public class Scheduler {

	@Autowired
	private OrderRatingService orderRatingService;

	@Autowired
	private StockDetailsService stockDetailsService;

	@Autowired
	private DiscountService discountService;

	private static final Logger LOGGER = LoggerFactory.getLogger(Scheduler.class);

	@Scheduled(cron = "0 0 0 * * ?")
	public void run() throws NotFoundException {
		LOGGER.info("Rating Scheduler -- Start: The time is now {}", new Date(System.currentTimeMillis()));
		runRatingScheduler(new Date());
		runDiscountScheduler(LocalDate.now());
		runStockExpiryScheduler(new Date());
	}

	private void runRatingScheduler(final Date date) throws NotFoundException {
		orderRatingService.calculateRating(CommonUtility.convetUtilDatetoLocalDate(date));
	}

	/**
	 * @param runDate
	 */
	private void runStockExpiryScheduler(final Date runDate) {
		stockDetailsService.moveQtyToExpiredState(runDate);
	}

	/**
	 *
	 * @param runDate
	 */
	private void runDiscountScheduler(final LocalDate runDate) {
		discountService.activateExpireDiscount(runDate);
	}

}
