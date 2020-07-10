package com.nice.jms.component;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.gson.JsonObject;
import com.nice.constant.Constant;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.dto.PushNotification;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeliveryBoyLocation;
import com.nice.model.DeviceDetail;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyLocationService;
import com.nice.service.DeviceDetailService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Apr-2020
 */
@Component("sendPushNotificationComponent")
public class SendPushNotificationComponent {

	private static final Logger LOGGER = LoggerFactory.getLogger(SendPushNotificationComponent.class);

	@Autowired
	private CustomerService customerService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private DeviceDetailService deviceDetailService;

	@Autowired
	private DeliveryBoyLocationService deliveryBoyLocationService;

	// @Autowired
	// private OrdersService ordersService;

	@Value("${application.name}")
	private String applicationName;

	public void addPushNotification(final PushNotification pushNotification) throws NotFoundException, ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(pushNotification.getType())
				&& NotificationQueueConstants.ACCEPT_ORDER_PUSH_NOTIFICATION.equalsIgnoreCase(pushNotification.getType())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(pushNotification.getDeliveryBoyIds()) && pushNotification.getOrderId() != null) {
			Long vendorId = 1l;
			Vendor vendor = vendorService.getVendorDetail(vendorId);
			// FCMRestHelper fcm = FCMRestHelper.getInstance();
			JsonObject dataObject = new JsonObject();
			dataObject.addProperty("PickUp Address", /* get vendor address from order */ " ");
			dataObject.addProperty("Drop Address", /* get customer address from order */" ");
			dataObject.addProperty("Order Amount", /* get amount from order */ " ");
			dataObject.addProperty("Payment Mode", /* get payment mode from order */ " ");
			dataObject.addProperty("title", applicationName);

			for (Long deliveryBoyId : pushNotification.getDeliveryBoyIds()) {
				/**
				 * estimated time for order delivery would be Max( vendor's product preparation
				 * time , delivery boy's reaching time ) + time required by delivery boy to
				 * reach from vendor's location to customer's location
				 */
				DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLatestLocation(deliveryBoyId);
				/**
				 * distance between vendor and delivery boy
				 */
				Double distance = CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(),
						deliveryBoyLocation.getLatitude().doubleValue(), deliveryBoyLocation.getLongitude().doubleValue());
				/**
				 * distance between vendor and customer
				 */
				Double customerLatidude = 21.594335;
				Double customerLongitude = 70.283898;
				Double distance1 = CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(), customerLatidude,
						customerLongitude);
				/**
				 * max ( approx. time(minute)/60 for hour , distance/speed in hour)
				 */
				Integer estimatedTime = (int) (Math.max(vendor.getApproxDeliveryTime() / 60d, distance / Constant.DELIVERY_BOY_AVERAGE_SPEED)
						+ distance1 / Constant.DELIVERY_BOY_AVERAGE_SPEED);

				dataObject.addProperty("Estimated Time", estimatedTime);

				UserLogin userLogin = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(deliveryBoyId, UserType.DELIVERY_BOY.name());
				DeviceDetail deviceDetail = deviceDetailService.getDeviceDetailByUserId(userLogin.getId());
				// String result = fcm.sendNotification(FCMRestHelper.TYPE_TO,
				// deviceDetail.getDeviceId(), dataObject);
				// JsonObject resultObject = new Gson().fromJson(result, JsonObject.class);
				// LOGGER.info("push notification result {}", resultObject);
			}
		}
	}
}
