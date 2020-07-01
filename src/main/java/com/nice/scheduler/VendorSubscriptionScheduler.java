package com.nice.scheduler;

import java.time.LocalDate;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.stereotype.Component;

import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.dto.Notification;
import com.nice.dto.VendorFilterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.VendorRepository;
import com.nice.service.UserLoginService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */

@Component
public class VendorSubscriptionScheduler {

	private static final Logger LOGGER = LoggerFactory.getLogger(VendorSubscriptionScheduler.class);

	@Autowired
	private VendorRepository vendorRepository;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Autowired
	private TokenStore tokenStore;

	@Scheduled(cron = "0 30 0 * * ?")
	public void expiredSubscription() throws ValidationException, NotFoundException {
		LOGGER.info("vendor subscription expired -- Start: The time is now {}", new Date(System.currentTimeMillis()));
		VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
		vendorFilterDTO.setSubscriptionEndDate(new Date());
		List<Vendor> vendors = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
		for (Vendor vendor : vendors) {
			vendor.setActive(false);
			vendor.setStatus(VendorStatus.EXPIRED.name());
			vendorRepository.save(vendor);
			UserLogin userLogin = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(vendor.getId(), UserType.VENDOR.name());
			userLogin.setActive(false);
			userLoginService.updateUserLogin(userLogin);
			revokeToken(userLogin.getEmail());
		}
	}

	private void revokeToken(final String userName) {
		LOGGER.info("Revoking token for user {}", userName);
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName("kody-client", userName);
		for (OAuth2AccessToken token : tokens) {
			tokenStore.removeAccessToken(token);
		}
		LOGGER.info("Successfully Revoked token for user {}", userName);
	}

	@Scheduled(cron = "0 45 0 * * ?")
	public void subscriptionExpireReminder() {
		LOGGER.info("vendor subscription expired -- Start: The time is now {}", new Date(System.currentTimeMillis()));
		VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
		LocalDate localDate = CommonUtility.convetUtilDatetoLocalDate(new Date());
		localDate = localDate.plusDays(7L);
		vendorFilterDTO.setSubscriptionEndDate(CommonUtility.convertLocalDateToUtilDate(localDate));
		List<Vendor> vendors = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
		for (Vendor vendor : vendors) {
			Notification notification = new Notification();
			notification.setEmail(vendor.getEmail());
			notification.setVendorId(vendor.getId());
			notification.setUserType(UserType.VENDOR.name());
			notification.setType(NotificationQueueConstants.VENDOR_SUBSCRIPTION_EXPIRY_REMINDER);
			jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
		}
	}

}
