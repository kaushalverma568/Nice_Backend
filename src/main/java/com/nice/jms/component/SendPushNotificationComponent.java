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
import com.nice.constant.Constant;
import com.nice.constant.NotificationMessageConstantsArabic;
import com.nice.constant.NotificationMessageConstantsEnglish;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.constant.WalletTransactionTypeEnum;
import com.nice.dto.CompanyResponseDTO;
import com.nice.dto.NotificationPayloadDto;
import com.nice.dto.PushNotificationDTO;
import com.nice.dto.WalletTrxDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeviceDetail;
import com.nice.model.Orders;
import com.nice.model.PushNotification;
import com.nice.model.PushNotificationReceiver;
import com.nice.model.Task;
import com.nice.model.Ticket;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.service.CompanyService;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.DeviceDetailService;
import com.nice.service.OrdersService;
import com.nice.service.PaymentDetailsService;
import com.nice.service.PushNotificationReceiverService;
import com.nice.service.PushNotificationService;
import com.nice.service.TaskService;
import com.nice.service.TicketService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.service.WalletTrxService;
import com.nice.util.CommonUtility;
import com.nice.util.FCMRestHelper;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Apr-2020
 */
@Component("sendPushNotificationComponent")
public class SendPushNotificationComponent {

	/**
	 *
	 */
	private static final String BODY = "body";
	private static final String PUSH_NOTIFICATION_RESULT = "push notification result {}";
	private static final Logger LOGGER = LoggerFactory.getLogger(SendPushNotificationComponent.class);
	private static final String TITLE = "title";
	private static final String ORDER_ID = "orderId";
	private static final String IMAGE = "image";
	private static final String DELIVERY_BOY_KEY = "AAAA3whS1Sc:APA91bHgG8IwQ1Vxvt4K-bot5pgT0NH68cXIpIbt8NtTBmCNFo4V2iO0kImfw-Q0OWuUNm0dmQhBOZcDNj0QQ-BY3qgB9B2q1oCFMt4sIgB4s__qDUBZc15LGV_E65zccWPCMAAv6vHW";
	private static final String CUSTOMER_KEY = "AAAAnEI3SCU:APA91bEDEXwP1bi1hwf6JN2jnMhloGQ06gU5fnsMsbTYsJUKueY8IR9wlEdq-DX9f3KJr5-yyoHLCSTwkinhJm3z1PTFzrfQiyrDdS-qw-CsVIx9I9pg-3NvLZfsxs0u8dlj5r6nTX5k";
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
	private TaskService taskService;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private PushNotificationService pushNotificationService;

	@Autowired
	private PaymentDetailsService paymentDetailsService;

	@Autowired
	private OrdersService ordersService;

	@Autowired
	private PushNotificationReceiverService pushNotificationReceiverService;

	@Autowired
	private CompanyService companyService;

	@Autowired
	private WalletTrxService walletTxnService;

	@Value("${application.name}")
	private String applicationName;

	public void addPushNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			acceptOrderNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.NEW_VENDOR_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			newVendorNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.NEW_DB_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			newDeliveryBoyNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.NEW_TICKET_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			newTicketNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.NEW_ORDER_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			newOrderNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.ORDER_DELIVERY_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			orderDeliveryNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.PLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER.equals(pushNotificationDTO.getType())) {
			placeOrderNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER.equals(pushNotificationDTO.getType())) {
			cancelOrderNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.REPLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER.equals(pushNotificationDTO.getType())) {
			orderReplacedSuccessfully(pushNotificationDTO);
		} else if (NotificationQueueConstants.RETURN_ORDER_PUSH_NOTIFICATION_CUSTOMER.equals(pushNotificationDTO.getType())) {
			orderReturnedSuccessfully(pushNotificationDTO);
		} else if (NotificationQueueConstants.DELIVER_ORDER_PUSH_NOTIFICATION_CUSTOMER.equals(pushNotificationDTO.getType())) {
			orderDeliverSuccessfully(pushNotificationDTO);
		} else if (NotificationQueueConstants.ORDER_STATUS_CHANGE_PUSH_NOTIFICATION_CUSTOMER.equals(pushNotificationDTO.getType())) {
			orderStatusUpdate(pushNotificationDTO);
		} else if (NotificationQueueConstants.REPLACE_ORDER_PUSH_NOTIFICATION_VENDOR.equals(pushNotificationDTO.getType())) {
			replaceRequestFromCustomerToVendor(pushNotificationDTO);
		} else if (NotificationQueueConstants.RETURN_ORDER_PUSH_NOTIFICATION_VENDOR.equals(pushNotificationDTO.getType())) {
			returnRequestFromCustomerToVendor(pushNotificationDTO);
		} else if (NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_VENDOR.equals(pushNotificationDTO.getType())) {
			cancelOrderNotificationToVendor(pushNotificationDTO);
		} else if (NotificationQueueConstants.PAYOUT.equals(pushNotificationDTO.getType())) {
			payoutNotification(pushNotificationDTO);
		} else if (NotificationQueueConstants.RESOLVE_TICKET_PUSH_NOTIFICATION.equals(pushNotificationDTO.getType())) {
			resolveTicketPushNotification(pushNotificationDTO);
		}

	}

	private void resolveTicketPushNotification(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (pushNotificationDTO.getTicketId() != null && (pushNotificationDTO.getVendorId() != null || pushNotificationDTO.getDeliveryBoyId() != null
				|| pushNotificationDTO.getCustomerId() != null)) {
			Long entityId;
			String entityType;
			if (pushNotificationDTO.getVendorId() != null) {
				entityType = UserType.VENDOR.name();
				entityId = pushNotificationDTO.getVendorId();
			} else if (pushNotificationDTO.getCustomerId() != null) {
				entityType = UserType.CUSTOMER.name();
				entityId = pushNotificationDTO.getCustomerId();
			} else {
				entityType = UserType.DELIVERY_BOY.name();
				entityId = pushNotificationDTO.getDeliveryBoyId();
			}
			String messageEnglish = NotificationMessageConstantsEnglish.resolveTicketByAdmin(pushNotificationDTO.getTicketId());
			String messageArabic = NotificationMessageConstantsArabic.resolveTicketByAdmin(pushNotificationDTO.getTicketId());
			PushNotification pushNotification = setPushNotification(entityId, entityType, messageEnglish, messageArabic, Constant.PAYOUT_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be entity will be admin and receiver will be either delivery
			 * boy or vendor
			 */
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(entityId, entityType);
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
			}
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			if (pushNotificationDTO.getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setModule(Constant.TICKET_MODULE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				if (entityType.equals(UserType.VENDOR.name())) {
					sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
				} else if (entityType.equals(UserType.DELIVERY_BOY.name())) {
					sendPushNotificationToDeliveryBoy(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
				} else if (entityType.equals(UserType.CUSTOMER.name())) {
					sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
				}

			}
		}

	}

	private void payoutNotification(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (pushNotificationDTO.getVendorId() != null || pushNotificationDTO.getDeliveryBoyId() != null) {
			Long entityId;
			String entityType;
			if (pushNotificationDTO.getVendorId() != null) {
				entityType = UserType.VENDOR.name();
				entityId = pushNotificationDTO.getVendorId();
			} else {
				entityType = UserType.DELIVERY_BOY.name();
				entityId = pushNotificationDTO.getDeliveryBoyId();
			}
			String messageEnglish = NotificationMessageConstantsEnglish.getPayoutMessage();
			String messageArabic = NotificationMessageConstantsArabic.getPayoutMessage();
			PushNotification pushNotification = setPushNotification(entityId, entityType, messageEnglish, messageArabic, Constant.PAYOUT_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be entity will be admin and receiver will be either delivery
			 * boy or vendor
			 */
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(entityId, entityType);
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
			}
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			if (pushNotificationDTO.getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setModule(Constant.PAYOUT_MODULE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
			}
		}
	}

	private void orderDeliveryNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.ORDER_DELIVERY_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getTaskId() != null) {
			Task task = taskService.getTaskDetail(pushNotificationDTO.getTaskId());
			DeliveryBoy deliveryBoy = task.getDeliveryBoy();
			String messageEnglish = NotificationMessageConstantsEnglish.getDeliveryOrderToVendorMessage(
					deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish()), task.getOrder().getId());
			String messageArabic = NotificationMessageConstantsArabic.getDeliveryOrderToVendorMessage(
					deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic()), task.getOrder().getId());
			PushNotification pushNotification = setPushNotification(task.getVendor().getId(), UserType.VENDOR.name(), messageEnglish, messageArabic,
					Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be entity who has created ticket and receiver will be admin
			 */
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(deliveryBoy.getId(), UserType.DELIVERY_BOY.name());
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(task.getVendor().getId(), UserType.VENDOR.name());
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
			if (task.getVendor().getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			dataObject.addProperty(ORDER_ID, task.getOrder().getId());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(task.getOrder().getId());
			notificationPayloadDto.setModule(Constant.ORDER_MODULE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
			}
		}
	}

	/**
	 * for sending new order notification to vendor
	 *
	 * @param pushNotificationDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void newOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.NEW_ORDER_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getOrderId() != null) {
			Orders orders = ordersService.getOrder(pushNotificationDTO.getOrderId());
			String messageEnglish = NotificationMessageConstantsEnglish.getNewOrderToVendorMessage(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.getNewOrderToVendorMessage(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(orders.getVendor().getId(), UserType.VENDOR.name(), messageEnglish, messageArabic,
					Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			/**
			 * here sender will be entity who has created ticket and receiver will be admin
			 */
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getCustomer().getId(), UserType.CUSTOMER.name());
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getVendor().getId(), UserType.VENDOR.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
			}

			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			if (orders.getVendor().getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(Constant.ORDER_MODULE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
			}
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
			String messageEnglish = NotificationMessageConstantsEnglish.getNewTicketMessage(entityNameEnglish);
			String messageArabic = NotificationMessageConstantsArabic.getNewTicketMessage(entityNameArabic);
			PushNotification pushNotification = setPushNotification(ticket.getEntityId(), ticket.getUserType(), messageEnglish, messageArabic,
					Constant.TICKET_MODULE);
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
			JsonObject notificationObject = new JsonObject();
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getTicketId());
			notificationPayloadDto.setModule(Constant.TICKET_MODULE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
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
				&& pushNotificationDTO.getDeliveryBoyId() != null) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(pushNotificationDTO.getDeliveryBoyId());
			String messageEnglish = NotificationMessageConstantsEnglish.getNewProfileMessage(deliveryBoy.getFirstNameEnglish()).concat(" ")
					.concat(deliveryBoy.getLastNameEnglish());
			String messageArabic = NotificationMessageConstantsArabic.getNewProfileMessage(deliveryBoy.getFirstNameArabic()).concat(" ")
					.concat(deliveryBoy.getLastNameArabic());
			PushNotification pushNotification = setPushNotification(deliveryBoy.getId(), UserType.DELIVERY_BOY.name(), messageEnglish, messageArabic,
					Constant.DELIVERY_BOY_PROFILE);
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
			JsonObject notificationObject = new JsonObject();
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getDeliveryBoyId());
			notificationPayloadDto.setModule(Constant.DELIVERY_BOY_PROFILE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
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
			String messageEnglish = NotificationMessageConstantsEnglish.getNewProfileMessage(vendor.getStoreNameEnglish());
			String messageArabic = NotificationMessageConstantsArabic.getNewProfileMessage(vendor.getStoreNameArabic());
			PushNotification pushNotification = setPushNotification(vendor.getId(), UserType.VENDOR.name(), messageEnglish, messageArabic,
					Constant.VENDOR_PROFILE);
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
			JsonObject notificationObject = new JsonObject();
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			CompanyResponseDTO company = companyService.getCompany(true);
			notificationObject.addProperty(BODY, message.toString());
			notificationObject.addProperty("icon", company.getCompanyImage());
			notificationObject.addProperty(IMAGE, company.getCompanyImage());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getVendorId());
			notificationPayloadDto.setModule(Constant.VENDOR_PROFILE);
			for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
				sendPushNotificationToAdminOrVendor(notificationObject, notificationPayloadDto, pushNotificationReceiver.getDeviceId());
			}
		}

	}

	public void acceptOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(pushNotificationDTO.getDeliveryBoyIds()) && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			for (Long deliveryBoyId : pushNotificationDTO.getDeliveryBoyIds()) {
				UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(deliveryBoyId, UserType.DELIVERY_BOY.name());
				List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
				String messageEnglish = NotificationMessageConstantsEnglish.getNewOrderMessage(pushNotificationDTO.getOrderId());
				String messageArabic = NotificationMessageConstantsArabic.getNewOrderMessage(pushNotificationDTO.getOrderId());
				PushNotification pushNotification = setPushNotification(deliveryBoyId, UserType.DELIVERY_BOY.name(), messageEnglish, messageArabic,
						Constant.ORDER_MODULE);
				pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
				if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
					message = message.append(pushNotification.getMessageEnglish());
				} else {
					message = message.append(pushNotification.getMessageArabic());
				}
				notificationObject.addProperty(BODY, message.toString());
				NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
				notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
				notificationPayloadDto.setModule(Constant.ORDER_MODULE);
				notificationPayloadDto.setTaskType(pushNotificationDTO.getTaskType());
				LOGGER.info("Delivery boy accept order notification for delivery boy: {}, order: {} and taskType:{}", deliveryBoyId,
						pushNotificationDTO.getOrderId(), pushNotificationDTO.getTaskType());
				List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
				for (DeviceDetail deviceDetail : deviceDetailList) {
					PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
							userLoginSender.getId(), userLoginReceiver.getId());
					pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
					sendPushNotificationToDeliveryBoy(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
				}
			}
		}
	}

	public void sendPushNotificationToDeliveryBoy(final JsonObject notificationObject, final Object dataObject, final String deviceId) {
		FCMRestHelper fcm = FCMRestHelper.getInstance(DELIVERY_BOY_KEY);
		notificationObject.addProperty(TITLE, applicationName);
		String result = fcm.sendNotifictaionAndData(FCMRestHelper.TYPE_TO, deviceId, notificationObject, dataObject, DELIVERY_BOY_KEY);
		JsonObject resultObject = new Gson().fromJson(result, JsonObject.class);
		LOGGER.info(PUSH_NOTIFICATION_RESULT, resultObject);
	}

	public void sendPushNotificationToAdminOrVendor(final JsonObject notificationObject, final Object dataObject, final String deviceId) {
		FCMRestHelper fcm = FCMRestHelper.getInstance(WEB_KEY);
		notificationObject.addProperty(TITLE, applicationName);
		String result = fcm.sendNotifictaionAndData(FCMRestHelper.TYPE_TO, deviceId, notificationObject, dataObject, WEB_KEY);
		JsonObject resultObject = new Gson().fromJson(result, JsonObject.class);
		LOGGER.info(PUSH_NOTIFICATION_RESULT, resultObject);
	}

	public void sendPushNotificationToCustomer(final JsonObject notificationObject, final Object dataObject, final String deviceId) {
		FCMRestHelper fcm = FCMRestHelper.getInstance(CUSTOMER_KEY);
		notificationObject.addProperty(TITLE, applicationName);
		String result = fcm.sendNotifictaionAndData(FCMRestHelper.TYPE_TO, deviceId, notificationObject, dataObject, CUSTOMER_KEY);
		JsonObject resultObject = new Gson().fromJson(result, JsonObject.class);
		LOGGER.info(PUSH_NOTIFICATION_RESULT, resultObject);
	}

	private PushNotification setPushNotification(final Long entityId, final String entityType, final String messageEnglish, final String messageArabic,
			final String module) {
		PushNotification pushNotification = new PushNotification();
		pushNotification.setEntityId(entityId);
		pushNotification.setEntityType(entityType);
		pushNotification.setActive(true);
		pushNotification.setSuccessAll(false);
		pushNotification.setMessageEnglish(messageEnglish);
		pushNotification.setMessageArabic(messageArabic);
		pushNotification.setModule(module);
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

	public void placeOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.PLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			Orders order = ordersService.getOrderById(pushNotificationDTO.getOrderId());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.getCreateOrderMessage(pushNotificationDTO.getOrderId(), order.getTotalOrderAmount());
			String messageArabic = NotificationMessageConstantsArabic.getCreateOrderMessage(pushNotificationDTO.getOrderId(), order.getTotalOrderAmount());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer place order notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void cancelOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& (NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
						|| NotificationQueueConstants.CANCEL_ORDER_BY_ADMIN_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType()))
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {

			Boolean orderCancelledByCustomer = true;
			if (NotificationQueueConstants.CANCEL_ORDER_BY_ADMIN_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())) {
				orderCancelledByCustomer = false;
			}

			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.getCancelOrderMessage(pushNotificationDTO.getOrderId(), orderCancelledByCustomer);
			String messageArabic = NotificationMessageConstantsArabic.getCancelOrderMessage(pushNotificationDTO.getOrderId(), orderCancelledByCustomer);
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer cancel order notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void orderStatusUpdate(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.ORDER_STATUS_CHANGE_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			Orders order = ordersService.getOrder(pushNotificationDTO.getOrderId());
			String messageEnglish = NotificationMessageConstantsEnglish.getOrderStatusUpdateMessageExceptDelivery(pushNotificationDTO.getOrderId(),
					order.getOrderStatus());
			String messageArabic = NotificationMessageConstantsArabic.getOrderStatusUpdateMessageExceptDelivery(pushNotificationDTO.getOrderId(),
					order.getOrderStatus());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer order status change notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void orderDeliverSuccessfully(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.DELIVER_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			Orders order = ordersService.getOrder(pushNotificationDTO.getOrderId());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.orderDeliverySuccessful(pushNotificationDTO.getOrderId(), order.getOrderStatus());
			String messageArabic = NotificationMessageConstantsArabic.orderDeliverySuccessful(pushNotificationDTO.getOrderId(), order.getOrderStatus());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Order deliver notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void orderReplacedSuccessfully(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.REPLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.orderItemReplaceSuccessful(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.orderItemReplaceSuccessful(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Order replacement notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void orderReturnedSuccessfully(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.RETURN_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.orderItemReturnSuccessful(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.orderItemReturnSuccessful(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Order return notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void rejectOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.REJECT_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.getRejectedOrderMessage(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.getRejectedOrderMessage(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer place order notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	/**
	 * replcae request raised notification to vendor
	 *
	 * @param pushNotificationDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void replaceRequestFromCustomerToVendor(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.REPLACE_ORDER_PUSH_NOTIFICATION_VENDOR.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			Orders orders = ordersService.getOrder(pushNotificationDTO.getOrderId());
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getCustomer().getId(), UserType.CUSTOMER.name());
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getVendor().getId(), UserType.VENDOR.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.replaceRequestInitiated(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.replaceRequestInitiated(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(orders.getCustomer().getId(), UserType.CUSTOMER.name(), messageEnglish, messageArabic,
					Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = orders.getCustomer();
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Order replacement notification for customer: {} and order: {} and vendor : {}", customer.getId(), pushNotificationDTO.getOrderId(),
					orders.getVendor().getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	/**
	 * return request raised notification to vendor
	 *
	 * @param pushNotificationDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void returnRequestFromCustomerToVendor(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.RETURN_ORDER_PUSH_NOTIFICATION_VENDOR.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			Orders orders = ordersService.getOrder(pushNotificationDTO.getOrderId());
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getCustomer().getId(), UserType.CUSTOMER.name());
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getVendor().getId(), UserType.VENDOR.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.returnRequestInitiated(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.returnRequestInitiated(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(orders.getCustomer().getId(), UserType.CUSTOMER.name(), messageEnglish, messageArabic,
					Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = orders.getCustomer();
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Order return request notification for customer: {} and order: {} to vendor : {}", customer.getId(), pushNotificationDTO.getOrderId(),
					orders.getVendor().getId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	/**
	 * cancel order by admin send notification to vendor
	 *
	 * @param pushNotificationDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void cancelOrderNotificationToVendor(final PushNotificationDTO pushNotificationDTO) throws NotFoundException, ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_VENDOR.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getOrderId() != null) {
			Orders orders = ordersService.getOrder(pushNotificationDTO.getOrderId());
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(orders.getVendor().getId(), UserType.VENDOR.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.cancelOrderByAdmin(pushNotificationDTO.getOrderId());
			String messageArabic = NotificationMessageConstantsArabic.cancelOrderByAdmin(pushNotificationDTO.getOrderId());
			PushNotification pushNotification = setPushNotification(orders.getVendor().getId(), UserType.VENDOR.name(), messageEnglish, messageArabic,
					Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = orders.getCustomer();
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Admin cancel order notification for vendor: {} and order: {}", orders.getVendor().getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}

	}

	public void refundOrderNotification(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.REFUND_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			WalletTrxDTO walletTrx = walletTxnService.getWalletTxnByOrderIdAndTxnType(pushNotificationDTO.getOrderId(),
					WalletTransactionTypeEnum.REFUND.name());
			String messageEnglish = NotificationMessageConstantsEnglish.getRefundOrderMessage(pushNotificationDTO.getOrderId(), walletTrx.getAmount());
			String messageArabic = NotificationMessageConstantsArabic.getRefundOrderMessage(pushNotificationDTO.getOrderId(), walletTrx.getAmount());
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer place order notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void acceptOrderNotificationToCustomer(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION_CUSTOMER.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(pushNotificationDTO.getDeliveryBoyId());
			String messageEnglish = NotificationMessageConstantsEnglish.getOrderAcceptedMessageToCustomer(pushNotificationDTO.getOrderId(),
					deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish()));
			String messageArabic = NotificationMessageConstantsArabic.getOrderAcceptedMessageToCustomer(pushNotificationDTO.getOrderId(),
					deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic()));
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer place order notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

	public void deactivationNotificationToCustomer(final PushNotificationDTO pushNotificationDTO) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotificationDTO.getType())
				&& NotificationQueueConstants.DEACTIVE_CUSTOMER_NOTIFICATION.equalsIgnoreCase(pushNotificationDTO.getType())
				&& pushNotificationDTO.getCustomerId() != null && pushNotificationDTO.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject notificationObject = new JsonObject();
			UserLogin userLoginSender = userLoginService.getSuperAdminLoginDetail();
			UserLogin userLoginReceiver = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(pushNotificationDTO.getCustomerId(),
					UserType.CUSTOMER.name());
			List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLoginReceiver.getId());
			String messageEnglish = NotificationMessageConstantsEnglish.profileSuspendedForCustomer();
			String messageArabic = NotificationMessageConstantsArabic.profileSuspendedForCustomer();
			PushNotification pushNotification = setPushNotification(pushNotificationDTO.getCustomerId(), UserType.CUSTOMER.name(), messageEnglish,
					messageArabic, Constant.ORDER_MODULE);
			pushNotification = pushNotificationService.addUpdatePushNotification(pushNotification);
			Customer customer = customerService.getCustomerDetails(userLoginReceiver.getEntityId());
			if (customer.getPreferredLanguage().equals("en")) {
				message = message.append(pushNotification.getMessageEnglish());
			} else {
				message = message.append(pushNotification.getMessageArabic());
			}
			notificationObject.addProperty(BODY, message.toString());
			NotificationPayloadDto notificationPayloadDto = new NotificationPayloadDto();
			notificationPayloadDto.setId(pushNotificationDTO.getOrderId());
			notificationPayloadDto.setModule(pushNotificationDTO.getModule());
			LOGGER.info("Customer place order notification for customer: {} and order: {}", customer.getId(), pushNotificationDTO.getOrderId());
			List<PushNotificationReceiver> pushNotificationReceivers = new ArrayList<>();
			for (DeviceDetail deviceDetail : deviceDetailList) {
				PushNotificationReceiver pushNotificationReceiver = setPushNotificationReceiver(pushNotification, deviceDetail.getDeviceId(),
						userLoginSender.getId(), userLoginReceiver.getId());
				pushNotificationReceivers.add(pushNotificationReceiverService.addUpdatePushNotificationReceiver(pushNotificationReceiver));
				sendPushNotificationToCustomer(notificationObject, notificationPayloadDto, deviceDetail.getDeviceId());
			}
		}
	}

}
