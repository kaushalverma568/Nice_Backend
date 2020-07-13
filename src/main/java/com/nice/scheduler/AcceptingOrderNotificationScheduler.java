package com.nice.scheduler;

import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.constant.NotificationQueueConstants;
import com.nice.dto.PushNotification;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoySendNotificationHistory;
import com.nice.repository.DeliveryBoySendNotificationHistoryRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */

@Component
public class AcceptingOrderNotificationScheduler {

	private static final Logger LOGGER = LoggerFactory.getLogger(AcceptingOrderNotificationScheduler.class);

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private DeliveryBoySendNotificationHistoryRepository deliveryBoySendNotificationHistoryRepository;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	// @Scheduled(fixedRate = 7000)
	public void AcceptingOrderNotification() throws NotFoundException, ValidationException {
// get order list which is appproved and assignmentTryCount < 3 and timer is less then current time
		// for(Orders order : orders)
		Long orderId = 1l;
		Long vendorId = 1l;
		List<Long> nextNearestDeliveryBoys = deliveryBoyService.getNextThreeNearestDeliveryBoysFromVendor(orderId, vendorId);
		/**
		 * if not a single delivery boy is logged in for accepting order then throw
		 * exception
		 */
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(nextNearestDeliveryBoys)) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.not.available", null));
		} else {
			PushNotification pushNotification = new PushNotification();
			pushNotification.setDeliveryBoyIds(nextNearestDeliveryBoys);
			pushNotification.setOrderId(orderId);
			pushNotification.setType(NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION);
			jmsQueuerService.sendPushNotification(NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION_QUEUE, pushNotification);
			/**
			 * update delivery boy notification history
			 */
			for (Long deliveryBoyId : nextNearestDeliveryBoys) {
				DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
				Optional<DeliveryBoySendNotificationHistory> optDeliveryBoyNotificationHistory = deliveryBoySendNotificationHistoryRepository
						.findByDeliveryBoy(deliveryBoy);
				if (optDeliveryBoyNotificationHistory.isPresent()) {
					optDeliveryBoyNotificationHistory.get().setOrderId(orderId);
					deliveryBoySendNotificationHistoryRepository.save(optDeliveryBoyNotificationHistory.get());
				} else {
					DeliveryBoySendNotificationHistory deliveryBoyNotificationHistory = new DeliveryBoySendNotificationHistory();
					deliveryBoyNotificationHistory.setActive(true);
					deliveryBoyNotificationHistory.setDeliveryBoy(deliveryBoy);
					deliveryBoyNotificationHistory.setOrderId(orderId);
					deliveryBoySendNotificationHistoryRepository.save(deliveryBoyNotificationHistory);
				}
			}
		}
	}
}
