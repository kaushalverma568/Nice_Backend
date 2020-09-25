package com.nice.scheduler;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.nice.constant.Constant;
import com.nice.constant.DeliveryType;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.OrderStatusEnum;
import com.nice.dto.PushNotificationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoySendNotificationHistory;
import com.nice.model.Orders;
import com.nice.repository.DeliveryBoySendNotificationHistoryRepository;
import com.nice.repository.OrdersRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */

@Component
public class AcceptingOrderNotificationScheduler {

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private OrdersService ordersService;

	@Autowired
	private OrdersRepository ordersRepository;

	@Autowired
	private DeliveryBoySendNotificationHistoryRepository deliveryBoySendNotificationHistoryRepository;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Scheduled(fixedRate = 70000)
	public void acceptingOrderNotification() throws NotFoundException, ValidationException {
		/**
		 * get order list which is appproved and notification not sended more then three
		 * times and next time we have to send is less then current time
		 */
		List<Orders> ordersList = ordersService.getAllQualifiedDeliveryOrdersForSendingNotification(OrderStatusEnum.CONFIRMED.getStatusValue(),
				DeliveryType.DELIVERY.getStatusValue(), Constant.MAX_ASSIGNMENT_TRY_COUNT, new Date(System.currentTimeMillis()));
		for (Orders orders : ordersList) {

			List<Long> nextNearestDeliveryBoys = deliveryBoyService.getNextThreeNearestDeliveryBoysFromVendor(orders.getId(), orders.getVendor().getId());
			/**
			 * if not a single delivery boy is logged in for accepting order then throw
			 * exception
			 */
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(nextNearestDeliveryBoys)) {
				throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.not.available", null));
				/**
				 * We will send notification to vendor / admin
				 */
			} else {
				PushNotificationDTO pushNotification = new PushNotificationDTO();
				pushNotification.setDeliveryBoyIds(nextNearestDeliveryBoys);
				pushNotification.setOrderId(orders.getId());
				pushNotification.setType(NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION);
				jmsQueuerService.sendPushNotification(NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE, pushNotification);
				/**
				 * update delivery boy notification history
				 */
				for (Long deliveryBoyId : nextNearestDeliveryBoys) {
					DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
					Optional<DeliveryBoySendNotificationHistory> optDeliveryBoyNotificationHistory = deliveryBoySendNotificationHistoryRepository
							.findByDeliveryBoy(deliveryBoy);
					if (optDeliveryBoyNotificationHistory.isPresent()) {
						optDeliveryBoyNotificationHistory.get().setOrderId(orders.getId());
						deliveryBoySendNotificationHistoryRepository.save(optDeliveryBoyNotificationHistory.get());
					} else {
						DeliveryBoySendNotificationHistory deliveryBoyNotificationHistory = new DeliveryBoySendNotificationHistory();
						deliveryBoyNotificationHistory.setActive(true);
						deliveryBoyNotificationHistory.setDeliveryBoy(deliveryBoy);
						deliveryBoyNotificationHistory.setOrderId(orders.getId());
						deliveryBoySendNotificationHistoryRepository.save(deliveryBoyNotificationHistory);
					}
				}
			}
			orders.setAssignmentTryCount(orders.getAssignmentTryCount() + 1);
			orders.setNotificationTimer(new Date(System.currentTimeMillis() + Constant.NOTIFICATION_SENDING_TIME_IN_MILIS));
			ordersRepository.save(orders);
		}
	}
}
