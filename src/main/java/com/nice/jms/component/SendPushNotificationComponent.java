package com.nice.jms.component;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.nice.constant.NotificationMessageConstantsArabic;
import com.nice.constant.NotificationMessageConstantsEnglish;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.dto.PushNotificationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeviceDetail;
import com.nice.model.PushNotification;
import com.nice.model.PushNotificationReceiver;
import com.nice.model.Ticket;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.DeviceDetailService;
import com.nice.service.PushNotificationReceiverService;
import com.nice.service.PushNotificationService;
import com.nice.service.TicketService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.FCMRestHelper;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Apr-2020
 */
@Component("sendPushNotificationComponent")
public class SendPushNotificationComponent {

	private static final Logger LOGGER = LoggerFactory.getLogger(SendPushNotificationComponent.class);
	private static final String MESSAGE = "message";
	private static final String DELIVERY_BOY_KEY = "";
	private static final String CUSTOMER_KEY = "";
	private static final String WEB_KEY = "AAAAGiFgaEY:APA91bFMLBoCm4dH8EMKu5dZ8mogU87S0Nh_fXWIn0w1xt03rCS-Q7KDHvzLKvoUDAiBZq-nb9DLufdeFc0qpBizALDfxPwh8UbuVQLf7D3euIdAbtD3AGjPynnIiPcUBrYV5RciCZ5k";

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private DeviceDetailService deviceDetailService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private TicketService ticketService;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private PushNotificationService pushNotificationService;

	@Autowired
	private PushNotificationReceiverService pushNotificationReceiverService;

	@Value("${application.name}")
	private String applicationName;

	public void addPushNotification(final PushNotificationDTO pushNotification) throws ValidationException, NotFoundException {
		if (NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equals(pushNotification.getType())) {
			acceptOrderNotification(pushNotification);
		} else if (NotificationQueueConstants.NEW_VENDOR_PUSH_NOTIFICATION.equals(pushNotification.getType())) {
			newVendorNotification(pushNotification);
		} else if (NotificationQueueConstants.NEW_DB_PUSH_NOTIFICATION.equals(pushNotification.getType())) {
			newDeliveryBoyNotification(pushNotification);
		} else if (NotificationQueueConstants.NEW_TICKET_PUSH_NOTIFICATION.equals(pushNotification.getType())) {
			newTicketNotification(pushNotification);
		}

	}

	/**
	 * for sending push notification to admin for new ticket
	 *
	 * @param pushNotificationDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void newTicketNotification(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.NEW_TICKET_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getTicketId() != null) {
			Ticket ticket = ticketService.getTicketDetail(pushNotificationDTO.getTicketId());
			String entityNameEnglish = null;
			String entityNameArabic = null;
			if (UserType.CUSTOMER.name().equals(ticket.getUserType())) {
				Customer customer = customerService.getCustomerDetails(ticket.getEntityId());
				entityNameEnglish = customer.getFirstName().concat(" ").concat(customer.getLastName());
				entityNameArabic = customer.getFirstName().concat(" ").concat(customer.getLastName());
			} else if (UserType.DELIVERY_BOY.name().equals(ticket.getUserType())) {
				DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(ticket.getEntityId());
				entityNameEnglish = deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish());
				entityNameArabic = deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic());
			} else if (UserType.VENDOR.name().equals(ticket.getUserType())) {
				Vendor vendor = vendorService.getVendorDetail(ticket.getEntityId());
				entityNameEnglish = vendor.getFirstNameEnglish().concat(" ").concat(vendor.getLastNameEnglish());
				entityNameArabic = vendor.getFirstNameArabic().concat(" ").concat(vendor.getLastNameArabic());
			}
			String messageEnglish = NotificationMessageConstantsEnglish.NEW_TICKET_VALIDATE.concat(entityNameEnglish);
			String messageArabic = NotificationMessageConstantsArabic.NEW_TICKET_VALIDATE.concat(entityNameArabic);
			PushNotification pushNotification = setPushNotification(ticket.getEntityId(), ticket.getUserType(), messageEnglish, messageArabic);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be entity who has created ticket and receiver will be admin
			 */
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(ticket.getEntityId(), ticket.getUserType());
			UserLogin userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
			}

			StringBuilder message = new StringBuilder();
			JsonObject dataObject = new JsonObject();
			JsonObject notificationObject = new JsonObject();
			if (pushNotificationDTO.getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(MESSAGE, message.toString());
			dataObject.addProperty("ticketId", pushNotificationDTO.getTicketId());
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, dataObject, pushNotificationReceiver.getDeviceId());
			}
		}
	}

	/**
	 * for sending delivery boy new profile notification to admin
	 *
	 * @param pushNotificationDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void newDeliveryBoyNotification(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.NEW_DB_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getVendorId() != null) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(pushNotificationDTO.getDeliveryBoyId());
			String messageEnglish = NotificationMessageConstantsEnglish.NEW_PROFILE_VALIDATE.concat(" ").concat(deliveryBoy.getFirstNameEnglish()).concat(" ")
					.concat(deliveryBoy.getLastNameEnglish());
			String messageArabic = NotificationMessageConstantsArabic.NEW_PROFILE_VALIDATE.concat(" ").concat(deliveryBoy.getFirstNameEnglish()).concat(" ")
					.concat(deliveryBoy.getLastNameEnglish());
			PushNotification pushNotification = setPushNotification(deliveryBoy.getId(), UserType.DELIVERY_BOY.name(), messageEnglish, messageArabic);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be delivery boy and receiver will be admin
			 */
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(deliveryBoy.getId(), UserType.DELIVERY_BOY.name());
			UserLogin userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
			}

			StringBuilder message = new StringBuilder();
			JsonObject dataObject = new JsonObject();
			JsonObject notificationObject = new JsonObject();
			if (pushNotificationDTO.getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(MESSAGE, message.toString());
			dataObject.addProperty("deliveryBoyId", pushNotificationDTO.getDeliveryBoyId());
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, dataObject, pushNotificationReceiver.getDeviceId());
			}
		}

	}

	/**
	 * send new vendor notification to admin
	 *
	 * @param pushNotificationDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void newVendorNotification(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.NEW_VENDOR_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getVendorId() != null) {
			Vendor vendor = vendorService.getVendorDetail(pushNotificationDTO.getVendorId());
			String messageEnglish = NotificationMessageConstantsEnglish.NEW_PROFILE_VALIDATE.concat(" ").concat(vendor.getStoreNameEnglish());
			String messageArabic = NotificationMessageConstantsArabic.NEW_PROFILE_VALIDATE.concat(" ").concat(vendor.getStoreNameArabic());
			PushNotification pushNotification = setPushNotification(vendor.getId(), UserType.VENDOR.name(), messageEnglish, messageArabic);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be vendor and receiver will be admin
			 */
			UserLogin userLoginReceiver = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(vendor.getId(), UserType.VENDOR.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
			}

			StringBuilder message = new StringBuilder();
			JsonObject dataObject = new JsonObject();
			JsonObject notificationObject = new JsonObject();
			if (pushNotificationDTO.getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(MESSAGE, message.toString());
			dataObject.addProperty("vendorId", pushNotificationDTO.getVendorId());
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, dataObject, pushNotificationReceiver.getDeviceId());
			}
		}

	}

	public void acceptOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(pushNotificationDTO.getDeliveryBoyIds()) && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject dataObject = new JsonObject();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			for (Long deliveryBoyId : pushNotificationDTO.getDeliveryBoyIds()) {
				UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(deliveryBoyId, UserType.DELIVERY_BOY.name());
				List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
				String messageEnglish = NotificationMessageConstantsEnglish.NEW_ORDER_DELIVERY;
				String messageArabic = NotificationMessageConstantsArabic.NEW_ORDER_DELIVERY;
				PushNotification pushNotification = setPushNotification(deliveryBoyId, UserType.DELIVERY_BOY.name(), messageEnglish, messageArabic);
				pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
				if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
					message = message.append(pushNotification.getMessageEnglish());
				} else {
					message = message.append(pushNotification.getMessageArabic());
				}
				notificationObject.addProperty(MESSAGE, message.toString());
				dataObject.addProperty("orderId", pushNotificationDTO.getOrderId());
				LOGGER.info("Delivery boy accept order notification for delivery boy: {} and order: {}", deliveryBoyId, pushNotificationDTO.getOrderId());
				List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
				for (DeviceDetail deviceDetail : deviceDetailList) {
					PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
							userLoginSender.getId(), userLoginReceiver.getId());
					pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
					sendPushNotificationToDeliveryBoy(notificationObject, dataObject, deviceDetail.getDeviceId());
				}
			}
		}
	}

	public void sendPushNotificationToDeliveryBoy(final JsonObject notificationObject, final JsonObject dataObject, final String deviceId) {
		FCMRestHelper fcm = FCMRestHelper.getInstance(DELIVERY_BOY_KEY);
		notificationObject.addProperty("title", applicationName);
		String result = fcm.sendNotifictaionAndData(FCMRestHelper.TYPE_TO, deviceId, notificationObject, dataObject);
		JsonObject resultObject = new Gson().fromJson(result, JsonObject.class);
		LOGGER.info("push notification result {}", resultObject);
	}

	public void sendPushNotificationToAdminOrVendor(final JsonObject notificationObject, final JsonObject dataObject, final String deviceId) {
		FCMRestHelper fcm = FCMRestHelper.getInstance(WEB_KEY);
		notificationObject.addProperty("title", applicationName);
		String result = fcm.sendNotifictaionAndData(FCMRestHelper.TYPE_TO, deviceId, notificationObject, dataObject);
		JsonObject resultObject = new Gson().fromJson(result, JsonObject.class);
		LOGGER.info("push notification result {}", resultObject);
	}

	private PushNotification setPushNotification(final Long entityId, final String entityType, final String messageEnglish, final String messageArabic) {
		PushNotification pushNotification = new PushNotification();
		pushNotification.setEntityId(entityId);
		pushNotification.setEntityType(entityType);
		pushNotification.setActive(true);
		pushNotification.setMessageEnglish(messageEnglish);
		pushNotification.setMessageArabic(messageArabic);
		return pushNotification;
	}

	private PushNotificationReceiver setPushNotificationReceiver(final PushNotification pushNotification, final String deviceId, final Long senderId,
			final Long receiverId) {
		PushNotificationReceiver pushNotificationReceiver = new PushNotificationReceiver();
		pushNotificationReceiver.setPushNotificationId(pushNotification.getId());
		pushNotificationReceiver.setActive(true);
		pushNotificationReceiver.setDeviceId(deviceId);
		pushNotificationReceiver.setMessageEnglish(pushNotification.getMessageEnglish());
		pushNotificationReceiver.setMessageArabic(pushNotification.getMessageArabic());
		pushNotificationReceiver.setSuccess(false);
		pushNotificationReceiver.setSenderId(senderId);
		pushNotificationReceiver.setReceiverId(receiverId);
		return pushNotificationReceiver;
	}

}
