package com.nice.service;

import java.io.IOException;
import java.security.GeneralSecurityException;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;

import com.nice.dto.CustomerDTO;
import com.nice.dto.CustomerPersonalDetailsDTO;
import com.nice.dto.CustomerResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Customer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface CustomerService {

	/**
	 * Add customer <br/>
	 * If isAuthorized is true then email will not trigger and customer and user login is activated. (For Facebook & Google)
	 * If isAuthorized is false then email will trigger and customer verified using email Only.(For normal users)
	 *
	 * @param  customerDto
	 * @param  isAuthorized
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	CustomerResponseDTO addCustomer(CustomerDTO customerDto, boolean isAuthorized) throws ValidationException, NotFoundException;

	/**
	 * update profile details of customer
	 *
	 * @param  customerPersonalDetailsDTO
	 * @return
	 * @throws NotFoundException
	 */
	Customer updateProfileDetails(CustomerPersonalDetailsDTO customerPersonalDetailsDTO) throws NotFoundException;

	/**
	 * Get customer detail by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	CustomerResponseDTO getCustomer(Long id) throws NotFoundException, ValidationException;

	/**
	 * Get customer list based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  sortByField
	 * @param  sortByDirection
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<Customer> getCustomerList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword, String sortByDirection,
			String sortByField) throws NotFoundException, ValidationException;

	/**
	 * Change status of customer (active/deActive)
	 *
	 * @param  id
	 * @param  isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeStatus(Long id, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * Check whether customer is exists or not
	 *
	 * @param  customersDto
	 * @return
	 */
	boolean isExists(CustomerDTO customersDto);

	/**
	 * Get customer details based on customerId : Specially for internal calls
	 *
	 * @param  customerId
	 * @return
	 * @throws NotFoundException
	 */
	Customer getCustomerDetails(Long customerId) throws NotFoundException;

	/**
	 * @param  customerId
	 * @throws NotFoundException
	 */
	void verifyEmail(Long customerId) throws NotFoundException;

	/**
	 * @param  active
	 * @return
	 */
	Long getActiveCustomer(boolean active);

	/**
	 * export customer list filter by activeRacords
	 *
	 * @param  activeRecords
	 * @param  httpServletResponse
	 * @throws IOException
	 */
	void exportCustomerList(Boolean activeRecords, HttpServletResponse httpServletResponse) throws IOException;

	/**
	 * @param  customerDTO
	 * @return
	 */
	boolean isPhoneExists(CustomerDTO customerDTO);

	/**
	 * @param  amount
	 * @param  customerId
	 * @throws NotFoundException
	 */
	void updateWalletBalance(Double amount, Long customerId) throws NotFoundException;

	/**
	 * @return
	 */
	Double getWalletBalance();

	/**
	 * @param deactiveCustomerNotification
	 * @param customerId
	 */
	void sendPushNotification(String deactiveCustomerNotification, Long customerId);

	/**
	 * @param  customerResponseDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void sendOtpForEmailVerification(CustomerResponseDTO customerResponseDTO) throws NotFoundException, ValidationException;
}