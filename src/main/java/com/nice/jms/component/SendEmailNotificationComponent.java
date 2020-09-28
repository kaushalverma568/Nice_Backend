/**
 *
 */
package com.nice.jms.component;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.HashMap;
import java.util.Map;

import javax.mail.MessagingException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.constant.EmailConstants;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.SendingType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.dto.CompanyResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.VendorBasicDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Customer;
import com.nice.model.Orders;
import com.nice.service.AssetService;
import com.nice.service.CompanyService;
import com.nice.service.CustomerService;
import com.nice.service.OrdersService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.EmailTemplatesEnum;
import com.nice.util.EmailUtil;

import net.sf.jasperreports.engine.JRException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component("sendEmailNotificationComponent")
public class SendEmailNotificationComponent {

	private static final String USER_TYPE = "userType";

	private static final String COMPANY_EMAIL = "companyEmail";

	private static final String APPLICATION_NAME = "applicationName";

	private static final String CUSTOMER_CARE_CONTACT = "customerCareContact";

	private static final String CUSTOMER_CARE_EMAIL = "customerCareEmail";

	private static final String BIG_LOGO = "applicationBigLogo";

	private static final String LOGO = "applicationLogo";

	private static final Logger LOGGER = LoggerFactory.getLogger(SendEmailNotificationComponent.class);
	private static final String CUSTOMER_NAME = "customerName";

	@Autowired
	private EmailUtil emailUtil;

	@Value("${customer.url}")
	private String customerUrl;

	@Value("${admin.url}")
	private String adminUrl;

	@Value("${application.name}")
	private String applicationName;

	@Value("${email.background}")
	private String emailBackgroundImage;

	@Value("${service.url}")
	private String serviceUrl;

	@Autowired
	private CompanyService companyService;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private AssetService assetService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrdersService ordersService;

	/**
	 * @param notification
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 * @throws ValidationException
	 * @throws JRException
	 */
	public void sendEmaillNotification(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		if (NotificationQueueConstants.CUSTOMER_REGISTRATION.equals(emailNotification.getType())) {
			customerRegistration(emailNotification);
		} else if (NotificationQueueConstants.FORGOT_PASS.equals(emailNotification.getType())) {
			forgotPassword(emailNotification);
		} else if (NotificationQueueConstants.EMAIL_VERIFICATION.equals(emailNotification.getType())) {
			emailVerification(emailNotification);
		} else if (NotificationQueueConstants.SEND_OTP.equals(emailNotification.getType())) {
			sendOtp(emailNotification);
		} else if (NotificationQueueConstants.VENDOR_SUBSCRIPTION_EXPIRY_REMINDER.equals(emailNotification.getType())) {
			subscriptionExpireReminder(emailNotification);
		} else if (NotificationQueueConstants.VENDOR_STATUS_CHANGE.equals(emailNotification.getType())) {
			sendEmailForChangeVendorStatus(emailNotification);
		} else if (NotificationQueueConstants.CANCEL_ORDER_EMAIL_NOTIFICATION_CUSTOMER.equals(emailNotification.getType())) {
			sendEmailForCancelOrderToCustomer(emailNotification);
		} else if (NotificationQueueConstants.RETURN_ORDER_EMAIL_NOTIFICATION_CUSTOMER.equals(emailNotification.getType())) {
			sendEmailForReturnOrderToCustomer(emailNotification);
		} else if (NotificationQueueConstants.REPLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER.equals(emailNotification.getType())) {
			sendEmailForReplaceOrderToCustomer(emailNotification);
		} else if (NotificationQueueConstants.PLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER.equals(emailNotification.getType())) {
			sendEmailForPlaceOrderToCustomer(emailNotification);
		}
	}

	private void customerRegistration(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getCustomerId() != null) {
			LOGGER.info("send email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailParameterMap.put("customerUrl", customerUrl);
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());

			final Customer customer = customerService.getCustomerDetails(emailNotification.getCustomerId());
			emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName() + " " + customer.getLastName());
			String welcomeEmailSubject = "Welcome to " + applicationName;
			emailUtil.sendEmail(welcomeEmailSubject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.WELCOME.name(),
					emailNotification.getLanguage());
		}
	}

	private void forgotPassword(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getOtp() != null && emailNotification.getEmail() != null) {
			LOGGER.info("send forgot Password email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailParameterMap.put("OtpValidity", String.valueOf(Constant.OTP_VALIDITY_TIME_IN_MIN));
			emailParameterMap.put("OTP", emailNotification.getOtp());
			if (UserType.CUSTOMER.name().equals(emailNotification.getUserType())) {
				emailParameterMap.put(USER_TYPE, "Customer");
				emailParameterMap.put("forgotPasswordUrl", customerUrl + "authentication/forgot-password?otp=" + emailNotification.getOtp() + "&userType="
						+ UserType.CUSTOMER.name() + "&type=" + UserOtpTypeEnum.EMAIL.name() + "&email=" + emailNotification.getEmail());
			} else if (UserType.USER.name().equals(emailNotification.getUserType())) {
				emailParameterMap.put(USER_TYPE, "User");
				emailParameterMap.put("forgotPasswordUrl", adminUrl + "auth/reset-password?otp=" + emailNotification.getOtp() + "&userType="
						+ emailNotification.getUserType() + "&type=" + UserOtpTypeEnum.EMAIL.name() + "&email=" + emailNotification.getEmail());
			} else if (UserType.DELIVERY_BOY.name().equals(emailNotification.getUserType())) {
				emailParameterMap.put(USER_TYPE, "Delivery Boy");
			}
			/**
			 * choose template according to sendingType (if sendingType is null then we choose both)
			 */
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(emailNotification.getSendingType())
					|| SendingType.BOTH.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(EmailConstants.FORGOT_CREDS_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
						EmailTemplatesEnum.FORGOT_PASSWORD_BOTH.name(), emailNotification.getLanguage());
			} else if (SendingType.OTP.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(EmailConstants.FORGOT_CREDS_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
						EmailTemplatesEnum.FORGOT_PASSWORD_OTP.name(), emailNotification.getLanguage());
			} else {
				emailUtil.sendEmail(EmailConstants.FORGOT_CREDS_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
						EmailTemplatesEnum.FORGOT_PASSWORD_LINK.name(), emailNotification.getLanguage());
			}
		}
	}

	private void emailVerification(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getOtp() != null && emailNotification.getEmail() != null) {
			LOGGER.info("email verification");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailParameterMap.put("verify", serviceUrl + "user/login/verify/email/" + emailNotification.getUserId() + "?lang=" + emailNotification.getLanguage()
					+ "&otp=" + emailNotification.getOtp());
			emailParameterMap.put("OTP", emailNotification.getOtp());
			emailParameterMap.put("OtpValidity", String.valueOf(Constant.OTP_VALIDITY_TIME_IN_MIN));

			if (UserType.CUSTOMER.name().equals(emailNotification.getUserType())) {
				emailParameterMap.put(USER_TYPE, "Customer");
			} else if (UserType.DELIVERY_BOY.name().equals(emailNotification.getUserType())) {
				emailParameterMap.put(USER_TYPE, "Delivery Boy");
			} else if (UserType.VENDOR.name().equals(emailNotification.getUserType())) {
				emailParameterMap.put(USER_TYPE, "Vendor");
			} else {
				emailParameterMap.put(USER_TYPE, "User");
			}

			/**
			 * choose template according to sendingType (if sendingType is null then we choose both)
			 */
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(emailNotification.getSendingType())
					|| SendingType.BOTH.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(EmailConstants.EMAIL_VERIFICATION_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
						EmailTemplatesEnum.EMAIL_VERIFICATION_BOTH.name(), emailNotification.getLanguage());
			} else if (SendingType.OTP.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(EmailConstants.EMAIL_VERIFICATION_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
						EmailTemplatesEnum.EMAIL_VERIFICATION_OTP.name(), emailNotification.getLanguage());
			} else {
				emailUtil.sendEmail(EmailConstants.EMAIL_VERIFICATION_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
						EmailTemplatesEnum.EMAIL_VERIFICATION_LINK.name(), emailNotification.getLanguage());
			}
		}
	}

	private void sendOtp(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put("otp", emailNotification.getOtp());
		String sendOtpSubject = applicationName + " : OTP";
		emailUtil.sendEmail(sendOtpSubject, emailNotification.getEmail(), paramMap, null, null, EmailTemplatesEnum.OTP_VERIFICATION.name(),
				emailNotification.getLanguage());
	}

	private void subscriptionExpireReminder(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException {
		Map<String, String> paramMap = new HashMap<>();
		String sendOtpSubject = applicationName + " Subscription Expire Reminder";
		paramMap.put("message", messageByLocaleService.getMessage("subscription.plan.reminder.message", null));
		emailUtil.sendEmail(sendOtpSubject, emailNotification.getEmail(), paramMap, null, null, EmailTemplatesEnum.SUBSCRIPTION_EXPIRE_REMINDER.name(),
				emailNotification.getLanguage());
	}

	private void sendEmailForChangeVendorStatus(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		VendorBasicDetailDTO vendor = vendorService.getVendorBasicDetailById(emailNotification.getVendorId());
		String message = null;
		if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus())) {
			message = messageByLocaleService.getMessage("vendor.approve.message", null);
		} else if (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
			message = messageByLocaleService.getMessage("vendor.suspend.message", new Object[] { applicationName, applicationName });
			message.concat(" ").concat(messageByLocaleService.getMessage("vendor.contact.customer.care", null));
		} else if (VendorStatus.REJECTED.getStatusValue().equals(vendor.getStatus())) {
			message = messageByLocaleService.getMessage("vendor.reject.messag", null);
		} else if (VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
			message = messageByLocaleService.getMessage("vendor.expire.message", null);
		} else {
			return;
		}
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = applicationName + " vendor update";
		emailParameterMap.put("vendorName",
				vendor.getPreferredLanguage().equals("en") ? vendor.getFirstNameEnglish().concat(" ").concat(vendor.getLastNameEnglish())
						: vendor.getFirstNameArabic().concat(" ").concat(vendor.getLastNameArabic()));
		emailParameterMap.put("message", message);
		emailUtil.sendEmail(subject, vendor.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.VENDOR_STATUS_CHANGE.name(),
				emailNotification.getLanguage());
	}

	private void sendEmailForReturnOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = applicationName + " Return Order";
		emailParameterMap.put("orderId", emailNotification.getOrderId().toString());
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.RETURN_ORDER_CUSTOMER.name(), emailLanguage);
	}

	private void sendEmailForCancelOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = applicationName + " Cancel Order";
		emailParameterMap.put("orderId", emailNotification.getOrderId().toString());
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.CANCEL_ORDER_CUSTOMER.name(), emailLanguage);
	}

	private void sendEmailForReplaceOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = applicationName + " Replace Order";
		emailParameterMap.put("orderId", emailNotification.getOrderId().toString());
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.REPLACE_ORDER_CUSTOMER.name(), emailLanguage);
	}

	private void sendEmailForPlaceOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = applicationName + " Place Order";
		emailParameterMap.put("orderId", emailNotification.getOrderId().toString());
		emailParameterMap.put("orderAmount", orders.getTotalOrderAmount().toString());
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.PLACE_ORDER_CUSTOMER.name(), emailLanguage);
	}

}