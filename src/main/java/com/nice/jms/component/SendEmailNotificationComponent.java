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
import com.nice.dto.Notification;
import com.nice.dto.VendorBasicDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
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

	private static final String MESSAGE = "message";

	private static final String SUBJECT2 = "subject";

	private static final String CONTENT2 = "content";

	private static final String ORDER_ID = "orderId";

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
	private MessageByLocaleService messageByLocaleService;

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
				emailParameterMap.put(CUSTOMER_NAME, vendor.getFirstNameEnglish() + " " + vendor.getLastNameEnglish());
				content = NotificationMessageConstantsEnglish.welcomeVendor(applicationNameEn);
				subject = NotificationMessageConstantsEnglish.welcomeSubject(applicationNameEn);
				applicationName = applicationNameEn;
			} else {
				emailParameterMap.put(CUSTOMER_NAME, vendor.getFirstNameArabic() + " " + vendor.getLastNameArabic());
				content = NotificationMessageConstantsArabic.welcomeVendor(applicationNameFr);
				subject = NotificationMessageConstantsArabic.welcomeSubject(applicationNameFr);
				applicationName = applicationNameFr;
			}
			emailParameterMap.put(CONTENT2, content);
			emailParameterMap.put(SUBJECT2, subject);
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailUtil.sendEmail(subject, vendor.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.WELCOME.name(), emailNotification.getLanguage());
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
				emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
				emailParameterMap.put("deliveryBoyName", deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish()));
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.deliveryBoyActivation());
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.ACCOUNT_ACCTIVATION_SUBJECT);
				subject = NotificationMessageConstantsEnglish.ACCOUNT_ACCTIVATION_SUBJECT;
			} else {
				emailParameterMap.put("deliveryBoyName", deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic()));
				emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.deliveryBoyActivation());
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.ACCOUNT_ACCTIVATION_SUBJECT);
				emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
				subject = NotificationMessageConstantsArabic.ACCOUNT_ACCTIVATION_SUBJECT;
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
					userName = vendor.getFirstNameEnglish().concat(" ").concat(vendor.getLastNameEnglish());
					content = NotificationMessageConstantsEnglish.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsEnglish.getPayoutSecondMessage();
					subject = NotificationMessageConstantsEnglish.VENDOR_PAYOUT_SUBJECT;
					applicationName = applicationNameEn;
				} else {
					userName = vendor.getFirstNameArabic().concat(" ").concat(vendor.getLastNameArabic());
					content = NotificationMessageConstantsArabic.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsArabic.getPayoutSecondMessage();
					subject = NotificationMessageConstantsArabic.VENDOR_PAYOUT_SUBJECT;
					applicationName = applicationNameFr;
				}
				emailNotification.setLanguage(vendor.getPreferredLanguage());
			} else {
				DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(emailNotification.getDeliveryBoyId());
				emailAddress = deliveryBoy.getEmail();
				if (deliveryBoy.getPreferredLanguage().equals("en")) {
					userName = deliveryBoy.getFirstNameEnglish().concat(" ").concat(deliveryBoy.getLastNameEnglish());
					content = NotificationMessageConstantsEnglish.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsEnglish.getPayoutSecondMessage();
					subject = NotificationMessageConstantsEnglish.DELIVERY_BOY_PAYOUT_SUBJECT;
					applicationName = applicationNameEn;
				} else {
					userName = deliveryBoy.getFirstNameArabic().concat(" ").concat(deliveryBoy.getLastNameArabic());
					content = NotificationMessageConstantsArabic.getPayoutMessage(paymentDetails.getPaidOn());
					secondLineContent = NotificationMessageConstantsArabic.getPayoutSecondMessage();
					subject = NotificationMessageConstantsArabic.DELIVERY_BOY_PAYOUT_SUBJECT;
					applicationName = applicationNameFr;
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
				content = NotificationMessageConstantsEnglish.welcome(applicationNameEn);
				subject = NotificationMessageConstantsEnglish.welcomeSubject(applicationNameEn);
				applicationName = applicationNameEn;
			} else {
				content = NotificationMessageConstantsArabic.welcome(applicationNameFr);
				subject = NotificationMessageConstantsArabic.welcomeSubject(applicationNameFr);
				applicationName = applicationNameFr;
			}
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
				emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.getResetMessage(applicationNameEn));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.resetPasswordSubject(applicationNameEn));
				emailParameterMap.put("otpValidity", NotificationMessageConstantsEnglish.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("linkValidity", NotificationMessageConstantsEnglish.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("instruction", NotificationMessageConstantsEnglish.getResetPasswordInsructionMessage());
				subject = NotificationMessageConstantsEnglish.resetPasswordSubject(applicationNameEn);
			} else {
				emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.getResetMessage(applicationNameFr));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.resetPasswordSubject(applicationNameFr));
				emailParameterMap.put("otpValidity", NotificationMessageConstantsArabic.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("linkValidity", NotificationMessageConstantsArabic.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("instruction", NotificationMessageConstantsArabic.getResetPasswordInsructionMessage());
				subject = NotificationMessageConstantsArabic.resetPasswordSubject(applicationNameFr);
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
			String userType = null;
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
				emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
				emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
				emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
				subject = NotificationMessageConstantsEnglish.EMAIL_VERIFICATION_SUBJECT;
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsEnglish.getEmailVerificationMessage(applicationNameEn));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsEnglish.EMAIL_VERIFICATION_SUBJECT);
				emailParameterMap.put("otpValidity", NotificationMessageConstantsEnglish.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("linkValidity", NotificationMessageConstantsEnglish.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("instruction", NotificationMessageConstantsEnglish.getInsructionMessage());
			} else {
				emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
				emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
				emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
				emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
				emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
				subject = NotificationMessageConstantsArabic.EMAIL_VERIFICATION_SUBJECT;
				emailParameterMap.put(CONTENT2, NotificationMessageConstantsArabic.getEmailVerificationMessage(applicationNameFr));
				emailParameterMap.put(SUBJECT2, NotificationMessageConstantsArabic.EMAIL_VERIFICATION_SUBJECT);
				emailParameterMap.put(USER_TYPE, userType);
				emailParameterMap.put("otpValidity", NotificationMessageConstantsArabic.getOtpValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("linkValidity", NotificationMessageConstantsArabic.getLinkValidityMessage(Constant.OTP_VALIDITY_TIME_IN_MIN));
				emailParameterMap.put("instruction", NotificationMessageConstantsArabic.getInsructionMessage());
			}
			emailParameterMap.put(USER_TYPE, userType);
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

	private void sendOtp(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put("otp", emailNotification.getOtp());
		String subject;
		if (emailNotification.getLanguage().equals("en")) {
			subject = NotificationMessageConstantsEnglish.sendOtpSubject(applicationNameEn);
		} else {
			subject = NotificationMessageConstantsArabic.sendOtpSubject(applicationNameFr);
		}
		emailUtil.sendEmail(subject, emailNotification.getEmail(), paramMap, null, null, EmailTemplatesEnum.OTP_VERIFICATION.name(),
				emailNotification.getLanguage());
	}

	private void subscriptionExpireReminder(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException {
		Map<String, String> paramMap = new HashMap<>();
		String subject;
		if (emailNotification.getLanguage().equals("en")) {
			subject = NotificationMessageConstantsEnglish.subscriptionExpireSubject(applicationNameEn);
		} else {
			subject = NotificationMessageConstantsArabic.subscriptionExpireSubject(applicationNameFr);
		}
		paramMap.put(MESSAGE, messageByLocaleService.getMessage("subscription.plan.reminder.message", null));
		emailUtil.sendEmail(subject, emailNotification.getEmail(), paramMap, null, null, EmailTemplatesEnum.SUBSCRIPTION_EXPIRE_REMINDER.name(),
				emailNotification.getLanguage());
	}

	private void sendEmailForChangeVendorStatus(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		Vendor vendor = vendorService.getVendorDetail(emailNotification.getVendorId());
		String subject = null;
		String message = null;
		String applicationName = null;
		if (emailNotification.getLanguage().equals("en")) {
			if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.approveVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.approveVendorProfileMessage();
			} else if (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.suspendVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.suspendVendorProfileMessage();
			} else if (VendorStatus.REJECTED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.rejectVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.rejectVendorProfileMessage();
			} else if (VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.expiredVendorSubscriptionSubject();
				VendorPayment vendorPayment = vendorPaymentService.getLatestVendorPaymentByVendorIdAndBusinessCategoryId(vendor.getId(),
						vendor.getSubscriptionPlan().getId());
				message = NotificationMessageConstantsEnglish.vendorSubscriptionExpiredMessage(vendorPayment.getAmount());
			} else if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsEnglish.resumeVendorProfileSubject();
				message = NotificationMessageConstantsEnglish.resumeVendorProfileMessage(applicationNameEn);
			} else {
				return;
			}
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
		} else {
			if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.approveVendorProfileSubject();
				message = NotificationMessageConstantsArabic.approveVendorProfileMessage();
			} else if (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.suspendVendorProfileSubject();
				message = NotificationMessageConstantsArabic.suspendVendorProfileMessage();
			} else if (VendorStatus.REJECTED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.rejectVendorProfileSubject();
				message = NotificationMessageConstantsArabic.rejectVendorProfileMessage();
			} else if (VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.expiredVendorSubscriptionSubject();
				VendorPayment vendorPayment = vendorPaymentService.getLatestVendorPaymentByVendorIdAndBusinessCategoryId(vendor.getId(),
						vendor.getSubscriptionPlan().getId());
				message = NotificationMessageConstantsArabic.vendorSubscriptionExpiredMessage(vendorPayment.getAmount());
			} else if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
				subject = NotificationMessageConstantsArabic.resumeVendorProfileSubject();
				message = NotificationMessageConstantsArabic.resumeVendorProfileMessage(applicationNameFr);
			} else {
				return;
			}
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
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
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
		} else {
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.returnOrderSubject(orders.getId());
			message = NotificationMessageConstantsArabic.returnOrderMessage(orders.getId());
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put("subject", subject);
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
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.cancelOrderSubject();
			message = NotificationMessageConstantsEnglish.cancelOrderMessage(orders.getId());
		} else {
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.cancelOrderSubject();
			message = NotificationMessageConstantsArabic.cancelOrderMessage(orders.getId());
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put("subject", subject);
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
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.replacementOrderSubject(orders.getId());
			message = NotificationMessageConstantsEnglish.replaceOrderMessage(orders.getId());
		} else {
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.replacementOrderSubject(orders.getId());
			message = NotificationMessageConstantsArabic.replaceOrderMessage(orders.getId());
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put("subject", subject);
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
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.placeOrderSubject();
			message = NotificationMessageConstantsEnglish.placeOrderMessage(orders.getId(), orders.getTotalOrderAmount());
			thankyouMessage = NotificationMessageConstantsEnglish.thankYouForShopping();
		} else {
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.placeOrderSubject();
			message = NotificationMessageConstantsArabic.placeOrderMessage(orders.getId(), orders.getTotalOrderAmount());
			thankyouMessage = NotificationMessageConstantsArabic.thankYouForShopping();
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put("subject", subject);
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
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsEnglish.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsEnglish.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsEnglish.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsEnglish.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameEn);
			subject = NotificationMessageConstantsEnglish.deliveryOrderSubject();
			message = NotificationMessageConstantsEnglish.orderDeliverySuccessful(orders.getId(), orders.getOrderStatus());
		} else {
			emailParameterMap.put("thanksRegards", NotificationMessageConstantsArabic.THANKS_REGARDS);
			emailParameterMap.put("customerSupportTeam", NotificationMessageConstantsArabic.CUSTOMER_SUPPORT_TEAM);
			emailParameterMap.put("customerCareNo", NotificationMessageConstantsArabic.CUSTOMER_CARE_NO);
			emailParameterMap.put("emailAddress", NotificationMessageConstantsArabic.EMAIL_ADDRESS);
			emailParameterMap.put(APPLICATION_NAME, applicationNameFr);
			subject = NotificationMessageConstantsArabic.deliveryOrderSubject();
			message = NotificationMessageConstantsArabic.orderDeliverySuccessful(orders.getId(), orders.getOrderStatus());
		}
		CompanyResponseDTO company = companyService.getCompany(false);
		emailParameterMap.put(LOGO, company.getCompanyImage());
		emailParameterMap.put("subject", subject);
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