package com.nice.scheduler;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.nice.constant.Constant;
import com.nice.constant.DeliveryType;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.TaskTypeEnum;
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
 * @date   : 30-Jun-2020
 */

@Component
public class AcceptingOrderNotificationScheduler {

	private static final Logger LOGGER = LoggerFactory.getLogger(AcceptingOrderNotificationScheduler.class);

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

	@Scheduled(fixedRate = 10000)
	public void acceptingOrderNotification() throws NotFoundException, ValidationException {
		/**
		 * get order list which is appproved, notification not sended more then three times and next time we have to send is
		 * less then current time ,we will try MAX_ASSIGNMENT_TRY_COUNT times
		 */

		List<Orders> ordersList = ordersService.getAllQualifiedDeliveryOrdersForSendingNotification(
				Arrays.asList(OrderStatusEnum.CONFIRMED.getStatusValue(), OrderStatusEnum.REPLACE_CONFIRMED.getStatusValue(),
						OrderStatusEnum.RETURN_CONFIRMED.getStatusValue()),
				DeliveryType.DELIVERY.getStatusValue(), Constant.MAX_ASSIGNMENT_TRY_COUNT, new Date());
		for (Orders orders : ordersList) {
			/**
			 * if we have tried exact MAX_ASSIGNMENT_TRY_COUNT times then we will simply increase a try count for maintaining retry
			 * button in UI
			 */
			if (!orders.getAssignmentTryCount().equals(Constant.MAX_ASSIGNMENT_TRY_COUNT)) {
				List<Long> nextNearestDeliveryBoys = deliveryBoyService.getNextThreeNearestDeliveryBoysFromVendor(orders.getId(), orders.getVendor().getId());
				/**
				 * if not a single delivery boy is logged in for accepting order then add logger
				 */
				if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(nextNearestDeliveryBoys)) {
					LOGGER.info("Delivery boy is not present for accepting order, order no : {}", orders.getId());
					/**
					 * We will send notification to vendor / admin
					 */
				} else {
					PushNotificationDTO pushNotification = new PushNotificationDTO();
					pushNotification.setDeliveryBoyIds(nextNearestDeliveryBoys);
					pushNotification.setOrderId(orders.getId());
					if (OrderStatusEnum.CONFIRMED.getStatusValue().equals(orders.getOrderStatus())) {
						pushNotification.setTaskType(TaskTypeEnum.DELIVERY.getTaskValue());
					} else if (OrderStatusEnum.REPLACE_CONFIRMED.getStatusValue().equals(orders.getOrderStatus())) {
						pushNotification.setTaskType(TaskTypeEnum.REPLACEMENT.getTaskValue());
					} else {
						pushNotification.setTaskType(TaskTypeEnum.RETURN.getTaskValue());
					}
					pushNotification.setType(NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION);
					jmsQueuerService.sendPushNotification(NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION_QUEUE, pushNotification);
					/**
					 * add entry in delivery boy notification history for this order
					 */
					for (Long deliveryBoyId : nextNearestDeliveryBoys) {
						DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
						DeliveryBoySendNotificationHistory deliveryBoyNotificationHistory = new DeliveryBoySendNotificationHistory();
						deliveryBoyNotificationHistory.setActive(true);
						deliveryBoyNotificationHistory.setDeliveryBoy(deliveryBoy);
						deliveryBoyNotificationHistory.setOrderId(orders.getId());
						deliveryBoySendNotificationHistoryRepository.save(deliveryBoyNotificationHistory);
					}
				}
				orders.setNotificationTimer(new Date(System.currentTimeMillis() + Constant.NOTIFICATION_SENDING_TIME_IN_MILIS));
			}
			orders.setAssignmentTryCount(orders.getAssignmentTryCount() + 1);
			ordersRepository.save(orders);
			/**
			 * send retry again push notification if condition match
			 */
			if (orders.getAssignmentTryCount().compareTo(Constant.MAX_ASSIGNMENT_TRY_COUNT) > 0) {
				PushNotificationDTO pushNotification = new PushNotificationDTO();
				pushNotification.setOrderId(orders.getId());
				pushNotification.setType(NotificationQueueConstants.RETRY_TO_SEARCH_DELIVERY_BOY);
				jmsQueuerService.sendPushNotification(NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE, pushNotification);
			}
		}
	}
}
