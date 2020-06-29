package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CustomerAddressDTO;
import com.nice.dto.CustomerAddressResponseDTO;
import com.nice.model.CustomerAddress;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Component
public class CustomerAddressMapper {

	public CustomerAddressResponseDTO toDto(final CustomerAddress customersAddress) {
		CustomerAddressResponseDTO customersAddressResponseDTO = new CustomerAddressResponseDTO();
		BeanUtils.copyProperties(customersAddress, customersAddressResponseDTO);
		customersAddressResponseDTO.setCustomerId(customersAddress.getCustomer().getId());
		customersAddressResponseDTO.setCountryId(customersAddress.getCountry().getId());
		customersAddressResponseDTO.setCountryName(customersAddress.getCountry().getName());
		if (customersAddress.getState() != null) {
			customersAddressResponseDTO.setStateId(customersAddress.getState().getId());
			customersAddressResponseDTO.setStateName(customersAddress.getState().getName());
		}
		if (customersAddress.getCity() != null) {
			customersAddressResponseDTO.setCityId(customersAddress.getCity().getId());
			customersAddressResponseDTO.setCityName(customersAddress.getCity().getName());
		}
		if (customersAddress.getPincode() != null) {
			customersAddressResponseDTO.setPincodeId(customersAddress.getPincode().getId());
			customersAddressResponseDTO.setPincodeValue(customersAddress.getPincode().getCodeValue());
		}
		return customersAddressResponseDTO;
	}

	public CustomerAddress toEntity(final CustomerAddressDTO customersAddressDto) {
		CustomerAddress customersAddress = new CustomerAddress();
		BeanUtils.copyProperties(customersAddressDto, customersAddress);
		return customersAddress;
	}

	public List<CustomerAddressResponseDTO> toDtos(final List<CustomerAddress> customersAddressList) {
		List<CustomerAddressResponseDTO> results = new ArrayList<>();
		for (CustomerAddress customerAddress : customersAddressList) {
			results.add(toDto(customerAddress));
		}
		return results;
	}
}