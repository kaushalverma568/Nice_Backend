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
import com.nice.constant.UserOtpTypeEnum;
import com.nice.dto.CompanyResponseDTO;
import com.nice.dto.Notification;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Customer;
import com.nice.service.CompanyService;
import com.nice.service.CustomerService;
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

	private static final String APPLICATION_NAME = "applicationName";

	private static final String CUSTOMER_CARE_CONTACT = "customerCareContact";

	private static final String CUSTOMER_CARE_EMAIL = "customerCareEmail";

	private static final String GROCERUS_BIG_LOGO = "grocerusBigLogo";

	private static final String GROCERUS_LOGO = "grocerusLogo";

	private static final Logger LOGGER = LoggerFactory.getLogger(SendEmailNotificationComponent.class);
	private static final String CUSTOMER_NAME = "customerName";

	@Autowired
	EmailUtil emailUtil;

	@Value("${customer.url}")
	private String customerUrl;

	@Value("${admin.url}")
	private String adminUrl;

	@Value("${application.name}")
	private String applicationName;

	@Value("${email.background}")
	private String emailBackgroundImage;

	@Autowired
	private CompanyService companyService;

	@Autowired
	private CustomerService customerService;

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
		} else if (NotificationQueueConstants.FORGOT_PASSWORD.equals(emailNotification.getType())) {
			forgotPassword(emailNotification);
		}
	}

	private void customerRegistration(final Notification emailNotification)
			throws NotFoundException, GeneralSecurityException, IOException, MessagingException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getCustomerId() != null) {
			LOGGER.info("send email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(GROCERUS_LOGO, company.getCompanyImage());
			emailParameterMap.put(GROCERUS_BIG_LOGO, CommonUtility.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getContactNo());
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailParameterMap.put("customerUrl", customerUrl);

			final Customer customer = customerService.getCustomerDetails(emailNotification.getCustomerId());
			emailParameterMap.put(CUSTOMER_NAME, customer.getFirstName() + " " + customer.getLastName());
			String welcomeEmailSubject = "Welcome to " + applicationName;
			emailUtil.sendEmail(welcomeEmailSubject, customer.getEmail(), emailParameterMap, null, null, EmailTemplatesEnum.WELCOME.name());
		}
	}

	private void forgotPassword(final Notification emailNotification) throws GeneralSecurityException, IOException, MessagingException, NotFoundException {
		final Map<String, String> emailParameterMap = new HashMap<>();
		if (emailNotification.getOtp() != null && emailNotification.getEmail() != null) {
			LOGGER.info("send forgot Password email");
			CompanyResponseDTO company = companyService.getCompany(true);
			emailParameterMap.put(GROCERUS_LOGO, company.getCompanyImage());
			emailParameterMap.put(GROCERUS_BIG_LOGO, CommonUtility.getGeneratedUrl(emailBackgroundImage, AssetConstant.COMPANY_DIR));
			emailParameterMap.put(CUSTOMER_CARE_EMAIL, company.getCustomerCareEmail());
			emailParameterMap.put(CUSTOMER_CARE_CONTACT, company.getContactNo());
			emailParameterMap.put(APPLICATION_NAME, applicationName);
			emailParameterMap.put("OtpValidity", String.valueOf(Constant.OTP_VALIDITY_TIME_IN_MIN));

			if (Constant.CUSTOMER.equalsIgnoreCase(emailNotification.getUserType())) {
				emailParameterMap.put("forgotPasswordUrl", customerUrl + "authentication/forgot-password?userId=" + emailNotification.getCustomerId() + "&otp="
						+ emailNotification.getOtp() + "&userType=" + Constant.CUSTOMER);
			} else if (Constant.ADMIN.equalsIgnoreCase(emailNotification.getUserType())) {
				emailParameterMap.put("forgotPasswordUrl", adminUrl + "authentication/reset-password?otp=" + emailNotification.getOtp() + "&userType="
						+ Constant.ADMIN + "&type=" + UserOtpTypeEnum.EMAIL.name() + "&userId=" + emailNotification.getCustomerId());
			}
			emailUtil.sendEmail(EmailConstants.FORGOT_CREDS_SUBJECT, emailNotification.getEmail(), emailParameterMap, null, null,
					EmailTemplatesEnum.RESET_PASSWORD.name());
		}
	}

}