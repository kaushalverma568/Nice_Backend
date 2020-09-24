package com.nice.jms.component;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.dto.PushNotificationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeviceDetail;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.service.DeviceDetailService;
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

	private static final String DELIVERY_BOY_KEY = "";
	private static final String CUSTOMER_KEY = "";
	private static final String WEB_KEY = "";

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private DeviceDetailService deviceDetailService;

	@Autowired
	private VendorService vendorService;

	@Value("${application.name}")
	private String applicationName;

	public void addPushNotification(final PushNotificationDTO pushNotification) throws ValidationException, NotFoundException {
		if (NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equals(pushNotification.getType())) {
			acceptOrderNotification(pushNotification);
		} else if (NotificationQueueConstants.NEW_VENDOR_PUSH_NOTIFICATION.equals(pushNotification.getType())) {
			newVendorNotification(pushNotification);
		}
	}

	private void newVendorNotification(final PushNotificationDTO pushNotification) throws NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotification.getType())
				&& NotificationQueueConstants.NEW_VENDOR_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotification.getType())
				&& pushNotification.getVendorId() != null) {
			Vendor vendor = vendorService.getVendorDetail(pushNotification.getVendorId());
			StringBuilder message = new StringBuilder();
			JsonObject dataObject = new JsonObject();
			JsonObject notificationObject = new JsonObject();
			message = message.append("You have receive new profile to validate ").append(vendor.getStoreNameEnglish());
			notificationObject.addProperty("message", message.toString());
		}

	}

	public void acceptOrderNotification(final PushNotificationDTO pushNotification) throws ValidationException, NotFoundException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotification.getType())
				&& NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotification.getType())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(pushNotification.getDeliveryBoyIds()) && pushNotification.getOrderId() != null) {
			StringBuilder message = new StringBuilder();
			JsonObject dataObject = new JsonObject();
			JsonObject notificationObject = new JsonObject();
			message = message.append("New Order for Delivery");
			notificationObject.addProperty("message", message.toString());
			dataObject.addProperty("orderId", pushNotification.getOrderId());
			for (Long deliveryBoyId : pushNotification.getDeliveryBoyIds()) {
				UserLogin userLogin = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(deliveryBoyId, UserType.DELIVERY_BOY.name());
				List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLogin.getId());
				LOGGER.info("Delivery boy accept order notification for delivery boy: {} and order: {}", deliveryBoyId, pushNotification.getOrderId());
				for (DeviceDetail deviceDetail : deviceDetailList) {
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

}
