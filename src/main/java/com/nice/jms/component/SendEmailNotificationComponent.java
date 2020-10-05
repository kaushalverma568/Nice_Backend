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
import com.nice.constant.NotificationMessageConstantsArabic;
import com.nice.constant.NotificationMessageConstantsEnglish;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.SendingType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.dto.CompanyResponseDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.VendorBasicDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.PaymentDetails;
import com.nice.model.Vendor;
import com.nice.model.VendorPayment;
import com.nice.service.AssetService;
import com.nice.service.CompanyService;
import com.nice.service.CustomerService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.PaymentDetailsService;
import com.nice.service.VendorPaymentService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.EmailTemplatesEnum;
import com.nice.util.EmailUtil;

import net.sf.jasperreports.engine.JRException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component("sendEmailNotificationComponent")
public class SendEmailNotificationComponent {

	private static final String WELCOME = "welcome";

	private static final String CUSTOMER_CARE_NO = "customerCareNo";

	private static final String EMAIL_ADDRESS = "emailAddress";

	private static final String CUSTOMER_SUPPORT_TEAM = "customerSupportTeam";

	private static final String THANKS_REGARDS = "thanksRegards";

	private static final String OTP_VALIDITY = "otpValidity";

	private static final String HELLO = "hello";

	private static final String INSTRUCTION = "instruction";

	private static final String LINK_VALIDITY = "linkValidity";

	private static final String SUBJECT = "subject";

	private static final String MESSAGE = "message";

	private static final String SUBJECT2 = SUBJECT;

	private static final String CONTENT2 = "content";

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
	private String applicationNameEn;

	@Value("${application.name.fr}")
	private String applicationNameFr;

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
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private PaymentDetailsService paymentDetailsService;

	@Autowired
	private OrdersService ordersService;

	@Autowired
	private VendorPaymentService vendorPaymentService;

	/**
	 * @param  notification
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
		} else if (NotificationQueueConstants.DELIVER_ORDER_EMAIL_NOTIFICATION_CUSTOMER.equals(emailNotification.getType())) {
			sendEmailForDeliverOrderToCustomer(emailNotification);
		} else if (NotificationQueueConstants.PAYOUT.equals(emailNotification.getType())) {
			sendEmailAfterPayout(emailNotification);
		} else if (NotificationQueueConstants.DELIVERY_BOY_ACCOUNT_ACTIVATION.equals(emailNotification.getType())) {
			sendEmailAfterDeliveryBoyAccountActivation(emailNotification);
		} else if (NotificationQueueConstants.VENDOR_REGISTRATION.equals(emailNotification.getType())) {
			vendorRegistration(emailNotification);
		} else if (NotificationQueueConstants.DELIVERY_BOY_REGISTRATION.equals(emailNotification.getType())) {
			deliveryBoyRegistration(emailNotification);
		}
	}

	private void vendorRegistration(final Notification emailNotification) throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getVendorId() != null) {
			String subject;
			String content;
			String applicationName;
			LOGGER.info("send vendor registration email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());

			final Vendor vendor = vendorService.getVendorDetail(emailNotification.getVendorId());

			if (emailNotification.getLanguage().equals("en")) {
				emailParameterMap.put(WELCOME, NotificationMessageConstantsEnglish.WELCOME);
				emailParameterMap.put(CUSTOMER_NAME, vendor.getFirstNameEnglish() + " " + vendor.getLastNameEnglish());
				content = NotificationMessageConstantsEnglish.welcomeVendor(applicationNameEn);
				subject = NotificationMessageConstantsEnglish.welcomeSubject(applicationNameEn);
				applicationName = applicationNameEn;
				emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
			} else {
				emailParameterMap.put(WELCOME, NotificationMessageConstantsArabic.WELCOME);
				emailParameterMap.put(CUSTOMER_NAME, vendor.getFirstNameArabic() + " " + vendor.getLastNameArabic());
				content = NotificationMessageConstantsArabic.welcomeVendor(applicationNameFr);
				subject = NotificationMessageConstantsArabic.welcomeSubject(applicationNameFr);
				applicationName = applicationNameFr;
				emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
			}
			emailParameterMap.put(CONTENT2, content);
			emailParameterMap.put(SUBJECT2, subject);
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailUtil.sendEmail(subject, vendor.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.WELCOME.name(), emailNotification.getLanguage());
		}
	}

	private void deliveryBoyRegistration(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getDeliveryBoyId() != null) {
			String subject;
			String content;
			String applicationName;
			LOGGER.info("send Delivery boy registration email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());

			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(emailNotification.getDeliveryBoyId());

			if (emailNotification.getLanguage().equals("en")) {
				emailParameterMap.put(WELCOME, NotificationMessageConstantsEnglish.WELCOME);
				emailParameterMap.put(CUSTOMER_NAME, deliveryBoy.getFirstNameEnglish() + " " + deliveryBoy.getLastNameEnglish());
				content = NotificationMessageConstantsEnglish.welcomeDeliveryBoy(applicationNameEn);
				subject = NotificationMessageConstantsEnglish.welcomeSubject(applicationNameEn);
				applicationName = applicationNameEn;
				emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
			} else {
				emailParameterMap.put(WELCOME, NotificationMessageConstantsArabic.WELCOME);
				emailParameterMap.put(CUSTOMER_NAME, deliveryBoy.getFirstNameArabic() + " " + deliveryBoy.getLastNameArabic());
				content = NotificationMessageConstantsArabic.welcomeDeliveryBoy(applicationNameFr);
				subject = NotificationMessageConstantsArabic.welcomeSubject(applicationNameFr);
				applicationName = applicationNameFr;
				emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
			}
			emailParameterMap.put(CONTENT2, content);
			emailParameterMap.put(SUBJECT2, subject);
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailUtil.sendEmail(subject, deliveryBoy.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.WELCOME.name(),
					emailNotification.getLanguage());
		}
	}

	private void sendEmailAfterDeliveryBoyAccountActivation(final Notification emailNotification)
			throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		LOGGER.info("send account activation email");
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getDeliveryBoyId() != null) {
			String subject;
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(emailNotification.getDeliveryBoyId());
			if (deliveryBoy.getPreferredLanguage().equals("en")) {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
				emailParameterMap.put("deliveryBoyName", deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish()));
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.deliveryBoyActivation());
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.ACCOUNT_ACCTIVATION_SUBJECT);
				subject = NotificationMessageConstantsEnglish.ACCOUNT_ACCTIVATION_SUBJECT;
				emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
			} else {
				emailParameterMap.put("deliveryBoyName", deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic()));
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.deliveryBoyActivation());
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.ACCOUNT_ACCTIVATION_SUBJECT);
				emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
				subject = NotificationMessageConstantsArabic.ACCOUNT_ACCTIVATION_SUBJECT;
				emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
			}
			emailUtil.sendEmail(subject, deliveryBoy.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.DELIVERY_BOY_ACCOUNT_ACTIVATION.name(),
					deliveryBoy.getPreferredLanguage());
		}
	}

	private void sendEmailAfterPayout(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		LOGGER.info("send payout email");
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getVendorId() != null || emailNotification.getDeliveryBoyId() != null) {
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
			String emailAddress;
			String subject;
			String content;
			String secondLineContent;
			String applicationName;
			String userName;
			PaymentDetails paymentDetails = paymentDetailsService.getPaymentDetailsDetail(emailNotification.getPaymentDetailsId());
			if (emailNotification.getVendorId() != null) {
				VendorBasicDetailDTO vendor = vendorService.getVendorBasicDetailById(emailNotification.getVendorId());
				emailAddress = vendor.getEmail();
				if (vendor.getPreferredLanguage().equals("en")) {
					emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
					emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
					emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
					emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
					userName = vendor.getFirstNameEnglish().concat(" ").concat(vendor.getLastNameEnglish());
					content = NotificationMessageConstantsEnglish.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsEnglish.getPayoutSecondMessage();
					subject = NotificationMessageConstantsEnglish.VENDOR_PAYOUT_SUBJECT;
					applicationName = applicationNameEn;
					emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
				} else {
					emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
					emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
					emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
					emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
					userName = vendor.getFirstNameArabic().concat(" ").concat(vendor.getLastNameArabic());
					content = NotificationMessageConstantsArabic.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsArabic.getPayoutSecondMessage();
					subject = NotificationMessageConstantsArabic.VENDOR_PAYOUT_SUBJECT;
					applicationName = applicationNameFr;
					emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
				}
				emailNotification.setLanguage(vendor.getPreferredLanguage());
			} else {
				DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(emailNotification.getDeliveryBoyId());
				emailAddress = deliveryBoy.getEmail();
				if (deliveryBoy.getPreferredLanguage().equals("en")) {
					emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
					emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
					emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
					emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
					userName = deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish());
					content = NotificationMessageConstantsEnglish.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsEnglish.getPayoutSecondMessage();
					subject = NotificationMessageConstantsEnglish.DELIVERY_BOY_PAYOUT_SUBJECT;
					applicationName = applicationNameEn;
					emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
				} else {
					emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
					emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
					emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
					emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
					userName = deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic());
					content = NotificationMessageConstantsArabic.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsArabic.getPayoutSecondMessage();
					subject = NotificationMessageConstantsArabic.DELIVERY_BOY_PAYOUT_SUBJECT;
					applicationName = applicationNameFr;
					emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
				}
				emailNotification.setLanguage(deliveryBoy.getPreferredLanguage());
			}
			emailParameterMap.put(CONTENT2, content);
			emailParameterMap.put("secondLineContent", secondLineContent);
			emailParameterMap.put("userName", userName);
			emailParameterMap.put(SUBJECT2, subject);
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailUtil.sendEmail(subject, emailAddress, emailParameterMap, null, null, EmailTemplatesEnum.PAYOUT.name(), emailNotification.getLanguage());
		}
	}

	private void customerRegistration(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getCustomerId() != null) {
			String subject;
			String content;
			String secondMessage;
			String applicationName;
			LOGGER.info("send customer registration email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put("customerUrl", customerUrl);
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());

			final Customer customer = customerService.getCustomerDetails(emailNotification.getCustomerId());
			emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName() + " " + customer.getLastName());
			if (emailNotification.getLanguage().equals("en")) {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				content = NotificationMessageConstantsEnglish.welcome(applicationNameEn);
				subject = NotificationMessageConstantsEnglish.welcomeSubject(applicationNameEn);
				secondMessage = NotificationMessageConstantsEnglish.welcomeSecondMessage();
				applicationName = applicationNameEn;
				emailParameterMap.put(WELCOME, NotificationMessageConstantsEnglish.WELCOME);
			} else {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				content = NotificationMessageConstantsArabic.welcome(applicationNameFr);
				subject = NotificationMessageConstantsArabic.welcomeSubject(applicationNameFr);
				applicationName = applicationNameFr;
				secondMessage = NotificationMessageConstantsArabic.welcomeSecondMessage();
				emailParameterMap.put(WELCOME, NotificationMessageConstantsArabic.WELCOME);
			}
			emailParameterMap.put("secondMessage", secondMessage);
			emailParameterMap.put(CONTENT2, content);
			emailParameterMap.put(SUBJECT2, subject);
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.WELCOME.name(),
					emailNotification.getLanguage());
		}
	}

	private void forgotPassword(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		if (emailNotification.getOtp() != null && emailNotification.getEmail() != null) {
			LOGGER.info("send forgot Password email");
			String subject = null;
			String userType = null;
			final Map<String, String> emailParameterMap = new HashMap<>();
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
			emailParameterMap.put("OTP", emailNotification.getOtp());
			if (UserType.CUSTOMER.name().equals(emailNotification.getUserType())) {
				if (emailNotification.getLanguage().equals("en")) {
					userType = NotificationMessageConstantsEnglish.USER_TYPE_CUSTOMER;
				} else {
					userType = NotificationMessageConstantsArabic.USER_TYPE_CUSTOMER;
				}
			} else if (UserType.USER.name().equals(emailNotification.getUserType())) {
				if (emailNotification.getLanguage().equals("en")) {
					userType = NotificationMessageConstantsEnglish.USER_TYPE_USER;
				} else {
					userType = NotificationMessageConstantsArabic.USER_TYPE_USER;
				}
				emailParameterMap.put("forgotPasswordUrl", adminUrl + "auth/reset-password?otp=" + emailNotification.getOtp() + "&userType="
						+ emailNotification.getUserType() + "&type=" + UserOtpTypeEnum.EMAIL.name() + "&email=" + emailNotification.getEmail());
			} else if (UserType.DELIVERY_BOY.name().equals(emailNotification.getUserType())) {
				if (emailNotification.getLanguage().equals("en")) {
					userType = NotificationMessageConstantsEnglish.USER_TYPE_DELIVERY_BOY;
				} else {
					userType = NotificationMessageConstantsArabic.USER_TYPE_DELIVERY_BOY;
				}
			}
			if (emailNotification.getLanguage().equals("en")) {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.getResetMessage(applicationNameEn));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.resetPasswordSubject(applicationNameEn));
				emailParameterMap.put(OTP_VALIDITY, NotificationMessageConstantsEnglish.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(LINK_VALIDITY, NotificationMessageConstantsEnglish.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(INSTRUCTION, NotificationMessageConstantsEnglish.getResetPasswordInsructionMessage());
				subject = NotificationMessageConstantsEnglish.resetPasswordSubject(applicationNameEn);
				emailParameterMap.put(HELLO, NotificationMessageConstantsEnglish.HELLO);
			} else {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.getResetMessage(applicationNameFr));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.resetPasswordSubject(applicationNameFr));
				emailParameterMap.put(OTP_VALIDITY, NotificationMessageConstantsArabic.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(LINK_VALIDITY, NotificationMessageConstantsArabic.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(INSTRUCTION, NotificationMessageConstantsArabic.getResetPasswordInsructionMessage());
				subject = NotificationMessageConstantsArabic.resetPasswordSubject(applicationNameFr);
				emailParameterMap.put(HELLO, NotificationMessageConstantsArabic.HELLO);
			}
			emailParameterMap.put(USER_TYPE, userType);
			/**
			 * choose template according to sendingType (if sendingType is null then we choose both)
			 */
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(emailNotification.getSendingType())
					|| SendingType.BOTH.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.FORGOT_PASSWORD_BOTH.name(),
						emailNotification.getLanguage());
			} else if (SendingType.OTP.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.FORGOT_PASSWORD_OTP.name(),
						emailNotification.getLanguage());
			} else {
				emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.FORGOT_PASSWORD_LINK.name(),
						emailNotification.getLanguage());
			}
		}
	}

	private void emailVerification(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getOtp() != null && emailNotification.getEmail() != null) {
			LOGGER.info("email verification");
			String subject = null;
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(LOGO, company.getCompanyImage());
			emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
			emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
			emailParameterMap.put("verify", serviceUrl + "user/login/verify/email/" + emailNotification.getUserId() + "?lang=" + emailNotification.getLanguage()
					+ "&otp=" + emailNotification.getOtp());
			emailParameterMap.put("OTP", emailNotification.getOtp());
			if (UserType.CUSTOMER.name().equals(emailNotification.getUserType())) {
				Customer customer = customerService.getCustomerDetails(emailNotification.getCustomerId());
				emailParameterMap.put("userName", customer.getFirstName() + " " + customer.getLastName());
			} else if (UserType.DELIVERY_BOY.name().equals(emailNotification.getUserType())) {
				DeliveryBoyResponseDTO deliveryBoy = deliveryBoyService.getDeliveryBoy(emailNotification.getDeliveryBoyId());
				if (emailNotification.getLanguage().equals("en")) {
					emailParameterMap.put("userName", deliveryBoy.getNameEnglish());
				} else {
					emailParameterMap.put("userName", deliveryBoy.getNameArabic());
				}
			} else if (UserType.VENDOR.name().equals(emailNotification.getUserType())) {
				Vendor vendor = vendorService.getVendorDetail(emailNotification.getVendorId());
				if (emailNotification.getLanguage().equals("en")) {
					emailParameterMap.put("userName", vendor.getFirstNameEnglish() + " " + vendor.getLastNameEnglish());
				} else {
					emailParameterMap.put("userName", vendor.getFirstNameArabic() + " " + vendor.getLastNameArabic());
				}
			}
			if (emailNotification.getLanguage().equals("en")) {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
				subject = NotificationMessageConstantsEnglish.EMAIL_VERIFICATION_SUBJECT;
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.getEmailVerificationMessage(applicationNameEn));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.EMAIL_VERIFICATION_SUBJECT);
				emailParameterMap.put(OTP_VALIDITY, NotificationMessageConstantsEnglish.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(LINK_VALIDITY, NotificationMessageConstantsEnglish.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(INSTRUCTION, NotificationMessageConstantsEnglish.getInsructionMessage());
				emailParameterMap.put(HELLO, NotificationMessageConstantsEnglish.HELLO);
				emailParameterMap.put("instructionLink", NotificationMessageConstantsEnglish.getInsructionMessageForLink());

			} else {
				emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
				subject = NotificationMessageConstantsArabic.EMAIL_VERIFICATION_SUBJECT;
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.getEmailVerificationMessage(applicationNameFr));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.EMAIL_VERIFICATION_SUBJECT);
				emailParameterMap.put(OTP_VALIDITY, NotificationMessageConstantsArabic.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(LINK_VALIDITY, NotificationMessageConstantsArabic.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put(INSTRUCTION, NotificationMessageConstantsArabic.getInsructionMessage());
				emailParameterMap.put("instructionLink", NotificationMessageConstantsArabic.getInsructionMessageForLink());
				emailParameterMap.put(HELLO, NotificationMessageConstantsArabic.HELLO);
			}
			/**
			 * choose template according to sendingType (if sendingType is null then we choose both)
			 */
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(emailNotification.getSendingType())
					|| SendingType.BOTH.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.EMAIL_VERIFICATION_BOTH.name(),
						emailNotification.getLanguage());
			} else if (SendingType.OTP.name().equalsIgnoreCase(emailNotification.getSendingType())) {
				emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.EMAIL_VERIFICATION_OTP.name(),
						emailNotification.getLanguage());
			} else {
				emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.EMAIL_VERIFICATION_LINK.name(),
						emailNotification.getLanguage());
			}
		}
	}

	private void sendOtp(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		LOGGER.info("send otp");
		String subject = null;
		String userType = null;
		CompanyResponseDTO company = companyService.getCompany(true);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put("OTP", emailNotification.getOtp());

		if (UserType.CUSTOMER.name().equals(emailNotification.getUserType())) {
			emailParameterMap.put(USER_TYPE, "Customer");
			if (emailNotification.getLanguage().equals("en")) {
				userType = NotificationMessageConstantsEnglish.USER_TYPE_CUSTOMER;
			} else {
				userType = NotificationMessageConstantsArabic.USER_TYPE_CUSTOMER;
			}
		} else if (UserType.DELIVERY_BOY.name().equals(emailNotification.getUserType())) {
			emailParameterMap.put(USER_TYPE, "Delivery Boy");
			if (emailNotification.getLanguage().equals("en")) {
				userType = NotificationMessageConstantsEnglish.USER_TYPE_DELIVERY_BOY;
			} else {
				userType = NotificationMessageConstantsArabic.USER_TYPE_DELIVERY_BOY;
			}
		} else if (UserType.VENDOR.name().equals(emailNotification.getUserType())) {
			emailParameterMap.put(USER_TYPE, "Vendor");
		} else {
			if (emailNotification.getLanguage().equals("en")) {
				userType = NotificationMessageConstantsEnglish.USER_TYPE_USER;
			} else {
				userType = NotificationMessageConstantsArabic.USER_TYPE_USER;
			}
		}

		if (emailNotification.getLanguage().equals("en")) {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.sendOtpSubject(applicationNameEn);
			emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.getOTPMessage());
			emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.EMAIL_VERIFICATION_SUBJECT);
			emailParameterMap.put(OTP_VALIDITY, NotificationMessageConstantsEnglish.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
			emailParameterMap.put(INSTRUCTION, NotificationMessageConstantsEnglish.getInsructionMessage());
			emailParameterMap.put(HELLO, NotificationMessageConstantsEnglish.HELLO);
		} else {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.sendOtpSubject(applicationNameFr);
			emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.getOTPMessage());
			emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.EMAIL_VERIFICATION_SUBJECT);
			emailParameterMap.put(USER_TYPE, userType);
			emailParameterMap.put(OTP_VALIDITY, NotificationMessageConstantsArabic.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
			emailParameterMap.put(INSTRUCTION, NotificationMessageConstantsArabic.getInsructionMessage());
			emailParameterMap.put(HELLO, NotificationMessageConstantsArabic.HELLO);
		}
		emailParameterMap.put(USER_TYPE, userType);

		emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.OTP_VERIFICATION.name(),
				emailNotification.getLanguage());
	}

	private void subscriptionExpireReminder(final Notification emailNotification)
			throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		Map<String, String> emailParameterMap = new HashMap<>();
		Vendor vendor = vendorService.getVendorDetail(emailNotification.getVendorId());
		VendorPayment vendorPayment = vendorPaymentService.getLatestVendorPaymentByVendorIdAndBusinessCategoryId(vendor.getId(),
				vendor.getSubscriptionPlan().getId());
		String subject = null;
		String message = null;
		String applicationName = null;
		if (emailNotification.getLanguage().equals("en")) {
			subject = NotificationMessageConstantsEnglish.subscriptionExpireSubject(applicationNameEn);
			message = NotificationMessageConstantsEnglish.vendorSubscriptionExpiredReminderMessage(vendorPayment.getAmount(),
					vendor.getSubscriptionPlanEndDate());
			applicationName = applicationNameEn;
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			subject = NotificationMessageConstantsArabic.subscriptionExpireSubject(applicationNameFr);
			message = NotificationMessageConstantsArabic.vendorSubscriptionExpiredReminderMessage(vendorPayment.getAmount(),
					vendor.getSubscriptionPlanEndDate());
			applicationName = applicationNameFr;
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put("vendorName",
				emailNotification.getLanguage().equals("en") ? vendor.getFirstNameEnglish().concat(" ").concat(vendor.getLastNameEnglish())
						: vendor.getFirstNameArabic().concat(" ").concat(vendor.getLastNameArabic()));
		emailParameterMap.put(MESSAGE, message);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(SUBJECT2, subject);
		emailUtil.sendEmail(subject, emailNotification.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.SUBSCRIPTION_EXPIRE_REMINDER.name(),
				emailNotification.getLanguage());
	}

	private void sendEmailForChangeVendorStatus(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		Vendor vendor = vendorService.getVendorDetail(emailNotification.getVendorId());
		String subject = null;
		String message = null;
		String secondMessage = null;
		String applicationName = null;
		if (emailNotification.getLanguage().equals("en")) {
			if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.approveVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.approveVendorProfileMessage();
			} else if (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.suspendVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.suspendVendorProfileMessage();
				secondMessage = NotificationMessageConstantsEnglish.suspendVendorProfileSecondMessage();
			} else if (VendorStatus.REJECTED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.rejectVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.rejectVendorProfileMessage();
				secondMessage = NotificationMessageConstantsEnglish.rejectVendorProfileSecondMessage();
			} else if (VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.expiredVendorSubscriptionSubject();
				VendorPayment vendorPayment = vendorPaymentService.getLatestVendorPaymentByVendorIdAndBusinessCategoryId(vendor.getId(),
						vendor.getSubscriptionPlan().getId());
				message = NotificationMessageConstantsEnglish.vendorSubscriptionExpiredMessage(vendorPayment.getAmount(), vendor.getSubscriptionPlanEndDate());
			} else if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.resumeVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.resumeVendorProfileMessage(applicationNameEn);
				secondMessage = NotificationMessageConstantsEnglish.resumeVendorProfileSecondMessage();
			} else {
				return;
			}
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.approveVendorProfileSubject();
				message = NotificationMessageConstantsArabic.approveVendorProfileMessage();
			} else if (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.suspendVendorProfileSubject();
				message = NotificationMessageConstantsArabic.suspendVendorProfileMessage();
				secondMessage = NotificationMessageConstantsArabic.suspendVendorProfileSecondMessage();
			} else if (VendorStatus.REJECTED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.rejectVendorProfileSubject();
				message = NotificationMessageConstantsArabic.rejectVendorProfileMessage();
				secondMessage = NotificationMessageConstantsArabic.rejectVendorProfileSecondMessage();
			} else if (VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.expiredVendorSubscriptionSubject();
				VendorPayment vendorPayment = vendorPaymentService.getLatestVendorPaymentByVendorIdAndBusinessCategoryId(vendor.getId(),
						vendor.getSubscriptionPlan().getId());
				message = NotificationMessageConstantsArabic.vendorSubscriptionExpiredMessage(vendorPayment.getAmount(), vendor.getSubscriptionPlanEndDate());
			} else if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.resumeVendorProfileSubject();
				message = NotificationMessageConstantsArabic.resumeVendorProfileMessage(applicationNameFr);
				secondMessage = NotificationMessageConstantsArabic.resumeVendorProfileSecondMessage();
			} else {
				return;
			}
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put("vendorName",
				emailNotification.getLanguage().equals("en") ? vendor.getFirstNameEnglish().concat(" ").concat(vendor.getLastNameEnglish())
						: vendor.getFirstNameArabic().concat(" ").concat(vendor.getLastNameArabic()));
		emailParameterMap.put(MESSAGE, message);
		emailParameterMap.put("secondMessage", secondMessage);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(SUBJECT2, subject);
		emailUtil.sendEmail(subject, vendor.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.VENDOR_STATUS_CHANGE.name(),
				emailNotification.getLanguage());
	}

	private void sendEmailForReturnOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		String applicationName = null;
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = null;
		String message = null;
		if ("en".equalsIgnoreCase(emailLanguage)) {
			subject = NotificationMessageConstantsEnglish.returnOrderSubject(orders.getId());
			message = NotificationMessageConstantsEnglish.returnOrderMessage(orders.getId());
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.returnOrderSubject(orders.getId());
			message = NotificationMessageConstantsArabic.returnOrderMessage(orders.getId());
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put(SUBJECT, subject);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName().concat(" ").concat(customer.getLastName()));
		emailParameterMap.put(MESSAGE, message);
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.ORDER_TEMPLATE.name(), emailLanguage);
	}

	private void sendEmailForCancelOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		String applicationName = null;
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = null;
		String message = null;
		if ("en".equalsIgnoreCase(emailLanguage)) {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.cancelOrderSubject(orders.getId());
			message = NotificationMessageConstantsEnglish.cancelOrderMessage(orders.getId());
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.cancelOrderSubject(orders.getId());
			message = NotificationMessageConstantsArabic.cancelOrderMessage(orders.getId());
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put(SUBJECT, subject);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(MESSAGE, message);
		emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName().concat(" ").concat(customer.getLastName()));
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.ORDER_TEMPLATE.name(), emailLanguage);
	}

	private void sendEmailForReplaceOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		String applicationName = null;
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = null;
		String message = null;
		if ("en".equalsIgnoreCase(emailLanguage)) {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.replacementOrderSubject(orders.getId());
			message = NotificationMessageConstantsEnglish.replaceOrderMessage(orders.getId());
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.replacementOrderSubject(orders.getId());
			message = NotificationMessageConstantsArabic.replaceOrderMessage(orders.getId());
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put(SUBJECT, subject);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(MESSAGE, message);
		emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName().concat(" ").concat(customer.getLastName()));
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.ORDER_TEMPLATE.name(), emailLanguage);
	}

	private void sendEmailForPlaceOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		String applicationName = null;
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = null;
		String message = null;
		String thankyouMessage = null;
		if ("en".equalsIgnoreCase(emailLanguage)) {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.placeOrderSubject(orders.getId());
			message = NotificationMessageConstantsEnglish.placeOrderMessage(orders.getId(),
					Double.sum(orders.getGrossOrderAmount(), orders.getDeliveryCharge()));
			thankyouMessage = NotificationMessageConstantsEnglish.thankYouForShopping();
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
			subject = NotificationMessageConstantsArabic.placeOrderSubject(orders.getId());
			message = NotificationMessageConstantsArabic.placeOrderMessage(orders.getId(),
					Double.sum(orders.getGrossOrderAmount(), orders.getDeliveryCharge()));
			thankyouMessage = NotificationMessageConstantsArabic.thankYouForShopping();
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put(SUBJECT, subject);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(MESSAGE, message);
		emailParameterMap.put("thankyou", thankyouMessage);
		emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName().concat(" ").concat(customer.getLastName()));
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.PLACE_ORDER_TEMPLATE.name(), emailLanguage);
	}

	private void sendEmailForDeliverOrderToCustomer(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		String applicationName = null;
		Orders orders = ordersService.getOrder(emailNotification.getOrderId());
		Customer customer = orders.getCustomer();
		String emailLanguage = customer.getPreferredLanguage();
		final Map<String, String> emailParameterMap = new HashMap<>();
		String subject = null;
		String message = null;
		if ("en".equalsIgnoreCase(emailLanguage)) {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.deliveryOrderSubject(orders.getId());
			message = NotificationMessageConstantsEnglish.orderDeliverySuccessful(orders.getId(), orders.getOrderStatus());
			emailParameterMap.put("dear", NotificationMessageConstantsEnglish.DEAR);
		} else {
			emailParameterMap.put(THANKS_REGARDS, NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put(CUSTOMER_SUPPORT_TEAM, NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put(CUSTOMER_CARE_NO, NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put(EMAIL_ADDRESS, NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.deliveryOrderSubject();
			message = NotificationMessageConstantsArabic.orderDeliverySuccessful(orders.getId(), orders.getOrderStatus());
			emailParameterMap.put("dear", NotificationMessageConstantsArabic.DEAR);
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put(SUBJECT, subject);
		emailParameterMap.put(BIG_LOGO, assetService.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
		emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
		emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getPhoneNumber());
		emailParameterMap.put(COMPANY_EMAIL, company.getCompanyEmail());
		emailParameterMap.put(APPLICATION_NAME, applicationName);
		emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName().concat(" ").concat(customer.getLastName()));
		emailParameterMap.put(MESSAGE, message);
		emailUtil.sendEmail(subject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.ORDER_TEMPLATE.name(), emailLanguage);
	}

}