package com.nice.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.CustomerStatus;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.Role;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.CustomerDTO;
import com.nice.dto.CustomerExport;
import com.nice.dto.CustomerResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.UserOtpDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CustomerMapper;
import com.nice.model.Customer;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.repository.CustomerRepository;
import com.nice.service.CustomerAddressService;
import com.nice.service.CustomerService;
import com.nice.service.OtpService;
import com.nice.service.UserLoginService;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Service(value = "customerService")
@Transactional(rollbackFor = Throwable.class)
public class CustomerServiceImpl implements CustomerService {

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomerServiceImpl.class);

	@Autowired
	private CustomerRepository customerRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CustomerMapper customerMapper;

	@Autowired
	private CustomerAddressService customerAddressService;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private OtpService otpService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Override
	public Long addCustomer(final CustomerDTO customersDTO, final boolean isAuthorized) throws ValidationException, NotFoundException {

		if (customersDTO.getPassword() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("password.required", null));
		}

		Customer customer = customerMapper.toEntity(customersDTO, 1L);

		/**
		 * Check if customer already exists, if so then lets only send him email again.
		 */
		Optional<Customer> optCustomer = customerRepository.findByEmailIgnoreCase(customersDTO.getEmail());
		if (optCustomer.isPresent() && !optCustomer.get().getEmailVerified().booleanValue()) {
			customer = optCustomer.get();
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmail(customersDTO.getEmail());
			if (optUserLogin.isPresent()) {
				sendOtpForEmailVerification(optUserLogin.get(), customer);
				return optUserLogin.get().getId();
			}
		}

		UserLogin userLogin = new UserLogin();
		if (isAuthorized) {
			customer.setEmailVerified(true);
			customer.setMobileVerified(false);
			customer.setStatus(CustomerStatus.ACTIVE.getStatusValue());
			userLogin.setActive(true);
		} else {
			customer.setEmailVerified(false);
			customer.setMobileVerified(false);
			customer.setStatus(CustomerStatus.PENDING.getStatusValue());
			userLogin.setActive(false);
		}

		Customer resultCustomer = customerRepository.save(customer);

		userLogin.setEntityId(resultCustomer.getId());
		userLogin.setEntityType(UserType.CUSTOMER.name());
		userLogin.setEmail(resultCustomer.getEmail());
		userLogin.setRole(Role.CUSTOMER.name());

		if ("FACEBOOK".equals(customersDTO.getRegisteredVia())) {
			userLogin.setFacebookKey(customersDTO.getPassword());
		} else if ("GOOGLE".equals(customersDTO.getRegisteredVia())) {
			userLogin.setGoogleKey(customersDTO.getPassword());
		} else {
			userLogin.setPassword(customersDTO.getPassword());
		}
		userLogin = userLoginService.addUserLogin(userLogin);
		/**
		 * Code to generate OTP and send that in email.
		 */
		if (!customer.getEmailVerified().booleanValue()) {
			sendOtpForEmailVerification(userLogin, resultCustomer);
		}

		/**
		 * Code to generate Otp and send email ends.
		 */
		return userLogin.getId();

	}

	/**
	 * @param  userLogin
	 * @param  resultCustomer
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws MessagingException
	 */
	private void sendOtpForEmailVerification(final UserLogin userLogin, final Customer resultCustomer) throws NotFoundException, ValidationException {
		UserOtpDto userOtpDto = new UserOtpDto();
		userOtpDto.setEmail(resultCustomer.getEmail());
		userOtpDto.setType(UserOtpTypeEnum.EMAIL.name());
		userOtpDto.setUserLoginId(userLogin.getId());
		UserOtp otp = otpService.generateOtp(userOtpDto);

		sendEmail(otp.getOtp(), userLogin.getId(), resultCustomer.getEmail());
	}

	private void sendEmail(final String otp, final Long userId, final String email) {
		Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setCustomerId(userId);
		notification.setEmail(email);
		notification.setType(NotificationQueueConstants.EMAIL_VERIFICATION);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	@Override
	public Customer updateCustomer(final CustomerDTO customerDTO) throws NotFoundException, ValidationException {
		if (customerDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("customer.id.not.null", null));
		}
		/**
		 * For validation whether customer exists or not
		 */
		Customer existingCustomer = getCustomerDetails(customerDTO.getId());
		Customer customer = customerMapper.toEntity(customerDTO, null);
		customer.setEmailVerified(existingCustomer.getEmailVerified());
		customer.setMobileVerified(existingCustomer.getMobileVerified());
		customer.setStatus(existingCustomer.getStatus());

		return customerRepository.save(customer);
	}

	@Override
	public CustomerResponseDTO getCustomer(final Long id) throws NotFoundException {
		Customer customer = getCustomerDetails(id);
		CustomerResponseDTO customerResponseDTO = customerMapper.toDto(customer);
		customerResponseDTO.setAddressList(customerAddressService.getAddressList(id));
		return customerResponseDTO;
	}

	@Override
	public Page<Customer> getCustomerList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword)
			throws NotFoundException, ValidationException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC, "id"));
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return customerRepository.findAllByActiveAndFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCaseOrPhoneNumberContainingIgnoreCase(
						activeRecords, searchKeyword, searchKeyword, searchKeyword, pageable);
			} else {
				return customerRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyword != null) {
				return customerRepository.findAllByFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCaseOrPhoneNumberContainingIgnoreCase(searchKeyword,
						searchKeyword, searchKeyword, pageable);
			} else {
				return customerRepository.findAll(pageable);
			}
		}
	}

	@Override
	public String changeStatus(final Long id, final Boolean active) throws NotFoundException, ValidationException {
		Customer existingCustomer = getCustomerDetails(id);
		LOGGER.info("Existing customer details {} ", existingCustomer);
		String userName = null;
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingCustomer.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "customer.active" : "customer.deactive", null));
		} else {
			if (Boolean.FALSE.equals(active)) {
				LOGGER.info("Deactivate customer");
				/**
				 * Delete all customer address once the customer is deactivated
				 */
				customerAddressService.deleteAllAddressByCustomer(existingCustomer);
				existingCustomer.setStatus(CustomerStatus.DE_ACTIVE.getStatusValue());
			} else {
				LOGGER.info("Activate customer");
				existingCustomer.setStatus(CustomerStatus.ACTIVE.getStatusValue());
			}

			LOGGER.info("Deactivate/activate user login");
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmail(existingCustomer.getEmail());
			if (optUserLogin.isPresent()) {
				UserLogin userLogin = optUserLogin.get();
				userLogin.setActive(active);
				userLoginService.updateUserLogin(userLogin);
				userName = userLogin.getEmail();
			}
			existingCustomer.setActive(active);
			customerRepository.save(existingCustomer);
		}
		return userName;
	}

	@Override
	public boolean isExists(final CustomerDTO customerDTO) {
		if (customerDTO.getId() != null) {
			/**
			 * At the time of update is customer with same name exist or not
			 */
			return customerRepository.findByEmailIgnoreCaseAndIdNot(customerDTO.getEmail(), customerDTO.getId()).isPresent();
		} else {
			/**
			 * findByAstarNameIgnoreCaseAndAstarIdNot At the time of create is customer with same name exist or not
			 */
			Optional<Customer> optCustomer = customerRepository.findByEmailIgnoreCase(customerDTO.getEmail());
			if (optCustomer.isPresent()) {
				/**
				 * If the customer is present and his email not verified, then we will be sending the verification link for him again,
				 * if the email is verified then we will be returning true.
				 */
				Customer customer = optCustomer.get();
				return customer.getEmailVerified();
			} else {
				return false;
			}

		}
	}

	@Override
	public Customer getCustomerDetails(final Long customerId) throws NotFoundException {
		return customerRepository.findById(customerId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("customer.not.found", new Object[] { customerId })));
	}

	@Override
	public void verifyEmail(final Long customerId) throws NotFoundException {
		Customer customer = getCustomerDetails(customerId);
		customer.setStatus(CustomerStatus.ACTIVE.getStatusValue());
		customer.setEmailVerified(true);
		customerRepository.save(customer);
	}

	@Override
	public void verifyPhoneNumber(final Long customerId, final String phoneNumber, final String otp) throws NotFoundException, ValidationException {
		if (customerRepository.findByPhoneNumberIgnoreCaseAndIdNot(phoneNumber, customerId).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("customer.phone.exists", null));
		}
		Long userId = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser().getId();
		if (otpService.verifyOtp(userId, UserOtpTypeEnum.SMS.name(), otp)) {
			Customer customer = getCustomerDetails(customerId);
			customer.setPhoneNumber(phoneNumber);
			customer.setMobileVerified(true);
			customerRepository.save(customer);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("user.otp.not.verified", new Object[] {}));
		}
	}

	@Override
	public boolean isUserLoginExists(final CustomerDTO customerDTO) {
		Optional<Customer> optCustomer = customerRepository.findByEmailIgnoreCase(customerDTO.getEmail());
		if (optCustomer.isPresent()) {
			return false;
		}
		Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmail(customerDTO.getEmail());
		if (optUserLogin.isPresent()) {
			return (!(customerDTO.getId() != null && optUserLogin.get().getEntityType().equals(UserType.CUSTOMER.name())
					&& customerDTO.getId().equals(optUserLogin.get().getEntityId())));
		} else {
			return false;
		}
	}

	@Override
	public Long getActiveCustomer(final boolean active) {
		return customerRepository.countByActive(active);
	}

	@Override
	public void exportCustomerList(final Boolean activeRecords, final HttpServletResponse httpServletResponse) throws IOException {
		List<Customer> customerList;
		List<CustomerExport> customerExportList = new ArrayList<>();
		if (activeRecords != null) {
			customerList = customerRepository.findAllByActive(activeRecords);
		} else {
			customerList = customerRepository.findAll();
		}
		for (Customer customer : customerList) {
			final CustomerExport customerExport = new CustomerExport();
			BeanUtils.copyProperties(customer, customerExport);
			customerExport.setName(customer.getFirstName() + ' ' + customer.getLastName());
			customerExport.setRegisteredOn(customer.getCreatedAt());
			customerExportList.add(customerExport);
		}
		final Object[] customerHeaderField = new Object[] { "Name", "Email", "Phone No.", "Gender", "Registered On", "Registered Via" };
		final Object[] customerDataField = new Object[] { "name", "email", "phoneNumber", "gender", "registeredOn", "registeredVia" };
		exportCSV.writeCSVFile(customerExportList, customerDataField, customerHeaderField, httpServletResponse);
	}

	@Override
	public boolean isPhoneExists(final CustomerDTO customerDTO) {
		if (customerDTO.getId() != null) {
			return customerRepository.findByPhoneNumberIgnoreCaseAndIdNot(customerDTO.getPhoneNumber(), customerDTO.getId()).isPresent();
		} else {
			return customerRepository.findByPhoneNumberIgnoreCase(customerDTO.getPhoneNumber()).isPresent();
		}
	}

	@Override
	public Customer getCustomerFromEmail(final String email) throws NotFoundException {
		Optional<Customer> optCustomer = customerRepository.findByEmailIgnoreCase(email);
		if (optCustomer.isPresent()) {
			return optCustomer.get();
		}
		throw new NotFoundException(messageByLocaleService.getMessage("customer.not.found.email", new Object[] { email }));
	}

}