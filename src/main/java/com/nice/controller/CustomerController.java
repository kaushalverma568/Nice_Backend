package com.nice.controller;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.dto.CustomerDTO;
import com.nice.dto.CustomerPersonalDetailsDTO;
import com.nice.dto.CustomerResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CustomerMapper;
import com.nice.model.Customer;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CustomerService;
import com.nice.validator.CustomerValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@RequestMapping(path = "/customer")
@RestController
public class CustomerController {

	/**
	 *
	 */
	private static final String CUSTOMER_UPDATE_MESSAGE = "customer.update.message";

	/**
	 *
	 */
	private static final String KODY_CLIENT = "kody-client";

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomerController.class);

	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CustomerService customerService;

	/**
	 * validator - to apply/check any type of validation regarding customers
	 */

	@Autowired
	private CustomerValidator customerValidator;

	@Autowired
	private TokenStore tokenStore;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(customerValidator);
	}

	@Autowired
	public CustomerMapper customerMapper;

	/**
	 * Add customer Whenever Login with OTP functionality exist then phone Number should be mandatory in customer sign-up
	 * Other wise validation will not work.
	 *
	 * @param  userId
	 * @param  customerDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	@PostMapping
	public ResponseEntity<Object> addCustomer(@RequestBody @Valid final CustomerDTO customerDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside add customer");
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("customers validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		CustomerResponseDTO responseCustomerDTO = customerService.addCustomer(customerDTO, false);
		LOGGER.info("Outside add customer ");
		/**
		 * send email verification email
		 */
		customerService.sendOtpForEmailVerification(responseCustomerDTO);
		/**
		 * send email verification email ends here
		 */
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(responseCustomerDTO)
				.setMessage(messageByLocaleService.getMessage("customer.create.message", null)).create();
	}

	/**
	 * Update profile details of customer
	 *
	 * @param  accessToken
	 * @param  customerPersonalDetailsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Customer','CAN_EDIT')")
	public ResponseEntity<Object> updateProfileDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final CustomerPersonalDetailsDTO customerPersonalDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update personal details for customer {}", customerPersonalDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("customer validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		final Customer resultCustomers = customerService.updateProfileDetails(customerPersonalDetailsDTO);
		LOGGER.info("Outside update personal details for customer");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(CUSTOMER_UPDATE_MESSAGE, null))
				.setData(customerMapper.toDto(resultCustomers)).create();
	}

	/**
	 * Get customer details based on id
	 *
	 * @param  customerId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{customerId}")
	@PreAuthorize("hasPermission('Customer','CAN_VIEW')")
	public ResponseEntity<Object> getCustomer(@RequestHeader("Authorization") final String accessToken, @PathVariable("customerId") final Long customerId)
			throws NotFoundException, ValidationException {
		final CustomerResponseDTO customerResponseDTO = customerService.getCustomer(customerId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("customer.detail.message", null))
				.setData(customerResponseDTO).create();
	}

	/**
	 * Get list of customer based on parameter
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Customer','CAN_VIEW')")
	public ResponseEntity<Object> getCustomerList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword,
			@RequestParam(name = "sortByDirection", required = false) final String sortByDirection,
			@RequestParam(name = "sortByField", required = false) final String sortByField) throws NotFoundException, ValidationException {
		final Page<Customer> resultCustomers = customerService.getCustomerList(pageNumber, pageSize, activeRecords, searchKeyword, sortByDirection,
				sortByField);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("customer.list.message", null))
				.setData(customerMapper.toDtos(resultCustomers.getContent())).setHasNextPage(resultCustomers.hasNext())
				.setHasPreviousPage(resultCustomers.hasPrevious()).setTotalPages(resultCustomers.getTotalPages()).setPageNumber(resultCustomers.getNumber() + 1)
				.setTotalCount(resultCustomers.getTotalElements()).create();
	}

	/**
	 * Change status of customer(active/deActive)
	 *
	 * @param  userId
	 * @param  customerId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{customerId}")
	@PreAuthorize("hasPermission('Customer','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("customerId") final Long customerId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of customer for id {} and status {}", customerId, active);
		String userName = customerService.changeStatus(customerId, active);
		if (userName != null) {
			revokeToken(userName.concat("!!").concat(UserType.CUSTOMER.name()));
		}
		customerService.sendPushNotification(NotificationQueueConstants.DEACTIVE_CUSTOMER_NOTIFICATION, customerId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(CUSTOMER_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * export customer list
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  httpServletResponse
	 * @param  activeRecords
	 * @return
	 * @throws IOException
	 */
	@GetMapping("/export/list")
	@PreAuthorize("hasPermission('Customer','CAN_VIEW')")
	public ResponseEntity<Object> exportCustomerList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) throws IOException {
		customerService.exportCustomerList(activeRecords, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("customer.list.message", null))
				.create();
	}

	/**
	 * revoke token for the user
	 *
	 * @param userName
	 * @param userId
	 */
	private void revokeToken(final String userName) {
		Long userId = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser().getId();
		LOGGER.info("Revoking token for user {} by userId {}", userName, userId);
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName(KODY_CLIENT, userName);
		for (OAuth2AccessToken token : tokens) {
			tokenStore.removeAccessToken(token);
		}
		LOGGER.info("Successfully Revoked token for user {} by userId {}", userName, userId);
	}

	@GetMapping("/wallet")
	public ResponseEntity<Object> getCustomerWalletBalance() {
		LOGGER.info("Inside get wallet for customer ");
		Double walletAmount = customerService.getWalletBalance();
		LOGGER.info("Outside get wallet for customer ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(walletAmount)
				.setMessage(messageByLocaleService.getMessage("customer.wallet.message", null)).create();
	}
}