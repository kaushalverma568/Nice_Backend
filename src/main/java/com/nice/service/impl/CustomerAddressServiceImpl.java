/**
 *
 */
package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CustomerAddressDTO;
import com.nice.dto.CustomerAddressResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CustomerAddressMapper;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.Customer;
import com.nice.model.CustomerAddress;
import com.nice.model.Pincode;
import com.nice.model.State;
import com.nice.repository.CustomerAddressRepository;
import com.nice.service.CityService;
import com.nice.service.CountryService;
import com.nice.service.CustomerAddressService;
import com.nice.service.CustomerService;
import com.nice.service.PincodeService;
import com.nice.service.StateService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service(value = "customerAddressService")
@Transactional(rollbackFor = Throwable.class)
public class CustomerAddressServiceImpl implements CustomerAddressService {

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomerAddressServiceImpl.class);

	@Autowired
	private CustomerAddressRepository customerAddressRepository;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CustomerAddressMapper customersAddressMapper;

	@Autowired
	private CountryService countryService;

	@Autowired
	private StateService stateService;

	@Autowired
	private CityService cityService;

	@Autowired
	private PincodeService pincodeService;

	@Override
	public Long addAddress(final Long customerId, final CustomerAddressDTO customersAddressDTO) throws ValidationException, NotFoundException {
		Customer customer = customerService.getCustomerDetails(customerId);
		Country country = countryService.getCountryDetails(customersAddressDTO.getCountryId());
		CustomerAddress customerAddress = customersAddressMapper.toEntity(customersAddressDTO);
		if (getCustomerAddressListBasedOnParams(true, customerId, null, null, null, null, null, null).isEmpty()) {
			customerAddress.setDefaultAddress(true);
		}
		customerAddress.setCustomer(customer);
		customerAddress.setCountry(country);
		State state = stateService.getStateDetails(customersAddressDTO.getStateId());
		customerAddress.setState(state);
		City city = cityService.getCityDetails(customersAddressDTO.getCityId());
		customerAddress.setCity(city);
		Pincode pincode = pincodeService.getPincodeDetails(customersAddressDTO.getPincodeId());
		customerAddress.setPincode(pincode);
		customerAddressRepository.save(customerAddress);
		return customerAddress.getId();
	}

	@Override
	public Long updateAddress(final Long customerId, final CustomerAddressDTO customersAddressDTO) throws NotFoundException, ValidationException {
		if (customersAddressDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("address.id.not.null", null));
		}
		getAddressDetails(customersAddressDTO.getId());
		return addAddress(customerId, customersAddressDTO);
	}

	@Override
	public CustomerAddressResponseDTO getAddress(final Long id) throws NotFoundException {
		CustomerAddress customersAddress = getAddressDetails(id);
		return customersAddressMapper.toDto(customersAddress);
	}

	@Override
	public boolean isExists(final CustomerAddressDTO customersAddressDTO) throws NotFoundException {
		Customer customer = customerService.getCustomerDetails(customersAddressDTO.getCustomerId());
		Pincode pincode = pincodeService.getPincodeDetails(customersAddressDTO.getPincodeId());

		if (customersAddressDTO.getId() != null) {
			/**
			 * While update check whether address is exists or not
			 */
			return customerAddressRepository
					.findByStreetNoAndBuildingNameAndLandmarkAndPincodeAndCustomerAndIdNot(customersAddressDTO.getStreetNo(),
							customersAddressDTO.getBuildingName(), customersAddressDTO.getLandmark(), pincode, customer, customersAddressDTO.getId())
					.isPresent();
		} else {
			/**
			 * While create check whether address is exists or not
			 */
			return customerAddressRepository.findByStreetNoAndBuildingNameAndLandmarkAndPincodeAndCustomer(customersAddressDTO.getStreetNo(),
					customersAddressDTO.getBuildingName(), customersAddressDTO.getLandmark(), pincode, customer).isPresent();
		}
	}

	@Override
	public CustomerAddress getAddressDetails(final Long addressId) throws NotFoundException {
		return customerAddressRepository.findById(addressId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("address.not.found", new Object[] { addressId })));
	}

	@Override
	public Page<CustomerAddress> getCustomerAddressList(final Long customerId, final Boolean activeRecords, final Integer pageNumber, final Integer pageSize)
			throws NotFoundException, ValidationException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		Customer customer = customerService.getCustomerDetails(customerId);
		if (activeRecords != null) {
			return customerAddressRepository.findAllByCustomerAndActive(customer, activeRecords, pageable);
		} else {
			return customerAddressRepository.findAllByCustomer(customer, pageable);
		}

	}

	@Override
	public List<CustomerAddress> getCustomerAddressListBasedOnParams(final Boolean activeRecords, final Long customerId, final Long countryId,
			final Long stateId, final Long cityId, final Long pincodeId, final Integer startIndex, final Integer pageSize) {
		return customerAddressRepository.getCustomerAddressListBasedOnParams(activeRecords, customerId, countryId, stateId, cityId, pincodeId, startIndex,
				pageSize);
	}

	@Override
	public List<CustomerAddressResponseDTO> getAddressList(final Long customerId) throws NotFoundException {
		Customer customer = customerService.getCustomerDetails(customerId);
		return customersAddressMapper.toDtos(customerAddressRepository.findAllByCustomer(customer));

	}

	@Override
	public void changeStatus(final Long customerAddressId, final Boolean active) throws ValidationException, NotFoundException {
		CustomerAddress existingCustomerAddress = getAddressDetails(customerAddressId);
		LOGGER.info("Existing customer address details {} ", existingCustomerAddress);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else {
			if (Boolean.TRUE.equals(active) && existingCustomerAddress.getCustomer().getActive().equals(Boolean.FALSE)) {
				throw new ValidationException(messageByLocaleService.getMessage("activate.customer", null));
			}
			if (Boolean.FALSE.equals(active)) {
				existingCustomerAddress.setDefaultAddress(false);
			}
			existingCustomerAddress.setActive(active);
			customerAddressRepository.save(existingCustomerAddress);
		}
	}

	@Override
	public void updateCustomerAddressDefault(final Long addressId) throws NotFoundException, ValidationException {
		CustomerAddress customerAddress = getAddressDetails(addressId);
		if (customerAddress.getActive().equals(Boolean.FALSE)) {
			throw new ValidationException(messageByLocaleService.getMessage("default.address.deactive", null));
		}
		customerAddress.setDefaultAddress(true);
		customerAddressRepository.save(customerAddress);
		Customer customer = customerService.getCustomerDetails(customerAddress.getCustomer().getId());
		List<CustomerAddress> customerAddresses = customerAddressRepository.findAllByCustomer(customer);
		for (CustomerAddress customerAddress2 : customerAddresses) {
			if (customerAddress2.isDefaultAddress() && !customerAddress2.getId().equals(addressId)) {
				customerAddress2.setDefaultAddress(false);
				customerAddressRepository.save(customerAddress2);
			}
		}
	}

	@Override
	public void deleteAddress(final Long customersAddressId) throws NotFoundException, ValidationException {
		CustomerAddress customerAddress = getAddressDetails(customersAddressId);
		if (customerAddress.isDefaultAddress()) {
			throw new ValidationException(messageByLocaleService.getMessage("delete.default.address", null));
		}
		customerAddressRepository.deleteById(customersAddressId);
	}

	@Override
	public void deleteAllAddressByCustomer(final Customer customer) {
		customerAddressRepository.deleteAllByCustomer(customer);
	}

	@Override
	public void deleteAllAddressByPincode(final Pincode pincode) {
		customerAddressRepository.deleteAllByPincode(pincode);
	}
}