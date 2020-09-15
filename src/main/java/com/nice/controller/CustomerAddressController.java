package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.DeleteMapping;
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

import com.nice.dto.CustomerAddressDTO;
import com.nice.dto.CustomerAddressResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CustomerAddressMapper;
import com.nice.model.CustomerAddress;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CustomerAddressService;
import com.nice.validator.CustomerAddressValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@RequestMapping(path = "/customer")
@RestController
public class CustomerAddressController {

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomerAddressController.class);

	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CustomerAddressService customerAddressService;

	/**
	 * validator - to apply/check any type of validation regarding customers
	 */

	@Autowired
	private CustomerAddressValidator customersAddressValidator;

	@Autowired
	private CustomerAddressMapper customerAddressMapper;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(customersAddressValidator);
	}

	/**
	 * Add customer Address
	 *
	 * @param  accessToken
	 * @param  customerAddressDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/address")
	@PreAuthorize("hasPermission('Customer','CAN_ADD')")
	public ResponseEntity<Object> addCustomersAddress(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final CustomerAddressDTO customerAddressDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add customer address  {}", customerAddressDTO);
		List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("customers validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the country & state in this, default it will be 1
		 */
		customerAddressDTO.setCountryId(1L);
		customerAddressDTO.setStateId(1L);

		/**
		 * Addition for country ends
		 */
		Long addressId = customerAddressService.addAddress(customerAddressDTO.getCustomerId(), customerAddressDTO);
		LOGGER.info("Outside add customer address for customer id {}", customerAddressDTO.getCustomerId());
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(addressId)
				.setMessage(messageByLocaleService.getMessage("address.create.message", null)).create();
	}

	/**
	 * Update customer address
	 *
	 * @param  accessToken
	 * @param  customerAddressDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/address")
	@PreAuthorize("hasPermission('Customer','CAN_EDIT')")
	public ResponseEntity<Object> updateCustomer(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final CustomerAddressDTO customerAddressDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update customer address {}", customerAddressDTO);

		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Customer address validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the country in this, default it will be 1
		 */
		customerAddressDTO.setCountryId(1L);
		customerAddressDTO.setStateId(1L);
		/**
		 * Addition for country ends
		 */
		Long addressId = customerAddressService.updateAddress(customerAddressDTO.getCustomerId(), customerAddressDTO);
		LOGGER.info("Outside update customers {}", customerAddressDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(addressId)
				.setMessage(messageByLocaleService.getMessage("address.update.message", null)).create();
	}

	/**
	 * Get customer address details based on id
	 *
	 * @param  accessToken
	 * @param  customerId
	 * @param  addressId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/address/{addressId}")
	@PreAuthorize("hasPermission('Customer','CAN_VIEW')")
	public ResponseEntity<Object> getCustomerAddress(@RequestHeader("Authorization") final String accessToken, @PathVariable("addressId") final Long addressId)
			throws NotFoundException {
		final CustomerAddressResponseDTO customerAddressResponseDTO = customerAddressService.getAddress(addressId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("address.detail.message", null))
				.setData(customerAddressResponseDTO).create();
	}

	/**
	 * Get list of customer Address based on customerId
	 *
	 * @param  accessToken
	 * @param  customerId
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{customerId}/address/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Customer','CAN_VIEW')")
	public ResponseEntity<Object> getCustomerAddressList(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("customerId") final Long customerId, @PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) throws NotFoundException, ValidationException {
		final Page<CustomerAddress> customerAddressList = customerAddressService.getCustomerAddressList(customerId, activeRecords, pageNumber, pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("address.list.message", null))
				.setData(customerAddressMapper.toDtos(customerAddressList.getContent())).setHasNextPage(customerAddressList.hasNext())
				.setHasPreviousPage(customerAddressList.hasPrevious()).setTotalPages(customerAddressList.getTotalPages())
				.setPageNumber(customerAddressList.getNumber() + 1).setTotalCount(customerAddressList.getTotalElements()).create();
	}

	/**
	 * Get customer address list based on parameters
	 *
	 * @param  accessToken
	 * @param  customerId
	 * @param  countryId
	 * @param  stateId
	 * @param  cityId
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 */
	@GetMapping("/address/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getCustomerAddressListBasedOnParams(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "customerId", required = false) final Long customerId,
			@RequestParam(name = "countryId", required = false) final Long countryId, @RequestParam(name = "stateId", required = false) final Long stateId,
			@RequestParam(name = "cityId", required = false) final Long cityId, @RequestParam(name = "pincodeId", required = false) final Long pincodeId,
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords) {
		final List<CustomerAddress> customerAddressList = customerAddressService.getCustomerAddressListBasedOnParams(activeRecords, customerId, countryId,
				stateId, cityId, pincodeId, 0, pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("address.list.message", null))
				.setData(customerAddressMapper.toDtos(customerAddressList)).create();
	}

	/**
	 * Change status of customer Address (active/deActive)
	 *
	 * @param  customersAddressId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@DeleteMapping("/address/{customersAddressId}")
	@PreAuthorize("hasPermission('Customer','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("customersAddressId") final Long customersAddressId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete cusotmer address of customer for address id {} ", customersAddressId);
		customerAddressService.deleteAddress(customersAddressId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("address.delete.message", null))
				.create();
	}

	/**
	 * Update customer address default based on id
	 *
	 * @param  accessToken
	 * @param  addressId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/address/default/{addressId}")
	@PreAuthorize("hasPermission('Customer','CAN_EDIT')")
	public ResponseEntity<Object> updateCustomerAddressDefault(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("addressId") final Long addressId) throws NotFoundException, ValidationException {
		customerAddressService.updateCustomerAddressDefault(addressId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("address.update.message", null))
				.create();
	}
}