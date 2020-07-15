package com.nice.service;

import java.io.IOException;
import java.security.GeneralSecurityException;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;

import com.nice.dto.CustomerDTO;
import com.nice.dto.CustomerResponseDTO;
import com.nice.dto.EmailUpdateDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Customer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
public interface CustomerService {

	/**
	 * Add customer <br/>
	 * If isAuthorized is true then email will not trigger and customer and user
	 * login is activated. (For Facebook & Google) If isAuthorized is false then
	 * email will trigger and customer verified using email Only.(For normal users)
	 *
	 * @param customerDto
	 * @param isAuthorized
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	Long addCustomer(CustomerDTO customerDto, boolean isAuthorized) throws ValidationException, NotFoundException;

	/**
	 * Update customer
	 *
	 * @param CustomerDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Customer updateCustomer(CustomerDTO customerDto) throws NotFoundException, ValidationException;

	/**
	 * Get customer detail by id
	 *
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	CustomerResponseDTO getCustomer(Long id) throws NotFoundException;

	/**
	 * Get customer list based on parameters
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyword
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<Customer> getCustomerList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword)
			throws NotFoundException, ValidationException;

	/**
	 * Change status of customer (active/deActive)
	 *
	 * @param id
	 * @param isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeStatus(Long id, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * Check whether customer is exists or not
	 *
	 * @param customersDto
	 * @return
	 */
	boolean isExists(CustomerDTO customersDto);

	/**
	 * Get customer details based on customerId : Specially for internal calls
	 *
	 * @param customerId
	 * @return
	 * @throws NotFoundException
	 */
	Customer getCustomerDetails(Long customerId) throws NotFoundException;

	/**
	 * @param customerId
	 * @param userId
	 * @param otp
	 * @param mobile
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void verifyPhoneNumber(Long customerId, String mobile, String otp) throws NotFoundException, ValidationException;

	/**
	 * @param customerId
	 * @throws NotFoundException
	 */
	void verifyEmail(Long customerId) throws NotFoundException;

	/**
	 * @param active
	 * @return
	 */
	Long getActiveCustomer(boolean active);

	/**
	 * export customer list filter by activeRacords
	 *
	 * @param activeRecords
	 * @param httpServletResponse
	 * @throws IOException
	 */
	void exportCustomerList(Boolean activeRecords, HttpServletResponse httpServletResponse) throws IOException;

	/**
	 * @param customerDTO
	 * @return
	 */
	boolean isPhoneExists(CustomerDTO customerDTO);

	/**
	 * add/update phone number
	 *
	 * @param phoneNumber
	 * @param customerId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String addUpdatePhoneNumber(String phoneNumber, String otp) throws NotFoundException, ValidationException;

	/***
	 * add/update email
	 *
	 * @param emailUpdateDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String addUpdateEmail(EmailUpdateDTO emailUpdateDTO) throws NotFoundException, ValidationException;

}