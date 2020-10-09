/**
 *
 */
package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.CustomerAddressDTO;
import com.nice.dto.CustomerAddressResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Area;
import com.nice.model.Customer;
import com.nice.model.CustomerAddress;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface CustomerAddressService {

	/**
	 * Add customer address
	 *
	 * @param  customerId
	 * @param  customersAddressDto
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Long addAddress(Long customerId, CustomerAddressDTO customersAddressDto) throws ValidationException, NotFoundException;

	/**
	 * Update customer address
	 *
	 * @param  customerId
	 * @param  customersAddressDto
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long updateAddress(final Long customerId, CustomerAddressDTO customersAddressDto) throws NotFoundException, ValidationException;

	/**
	 * Get address list based on customerId
	 *
	 * @param  customerId
	 * @param  activeRecords
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<CustomerAddress> getCustomerAddressList(Long customerId, Boolean activeRecords, Integer pageNumber, Integer pageSize)
			throws NotFoundException, ValidationException;

	/**
	 * Get customer address by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	CustomerAddressResponseDTO getAddress(Long id) throws NotFoundException;

	/**
	 * @param  customersAddressDTO
	 * @return
	 * @throws NotFoundException
	 */
	boolean isExists(CustomerAddressDTO customersAddressDTO) throws NotFoundException;

	/**
	 * To get details of address
	 *
	 * @param  addressId
	 * @return
	 * @throws NotFoundException
	 */
	CustomerAddress getAddressDetails(Long addressId) throws NotFoundException;

	/**
	 * Change status of customer Address (active/deActive)
	 *
	 * @param  customerId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long customerAddressId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * Get Address list based on customerId without pagination : Specially for internal call
	 *
	 * @param  customerId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<CustomerAddressResponseDTO> getAddressList(Long customerId) throws NotFoundException;

	/**
	 * Update customer address default
	 *
	 * @param  addressId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateCustomerAddressDefault(Long addressId) throws NotFoundException, ValidationException;

	/**
	 * @param  activeRecords
	 * @param  customerId
	 * @param  countryId
	 * @param  stateId
	 * @param  cityId
	 * @param  areaId
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<CustomerAddress> getCustomerAddressListBasedOnParams(Boolean activeRecords, Long customerId, Long countryId, Long stateId, Long cityId, Long areaId,
			Integer startIndex, Integer pageSize);

	/**
	 * @param  customersAddressId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void deleteAddress(Long customersAddressId) throws NotFoundException, ValidationException;

	/**
	 * @param customer
	 */
	void deleteAllAddressByCustomer(Customer customer);

	/**
	 * Delete all customer address by area
	 *
	 * @param existingArea
	 */
	void deleteAllAddressByArea(Area area);
}