package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
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
		Locale locale = LocaleContextHolder.getLocale();
		CustomerAddressResponseDTO customersAddressResponseDTO = new CustomerAddressResponseDTO();
		BeanUtils.copyProperties(customersAddress, customersAddressResponseDTO);
		customersAddressResponseDTO.setCustomerId(customersAddress.getCustomer().getId());
		customersAddressResponseDTO.setCountryId(customersAddress.getCountry().getId());
		if (locale.getLanguage().equals("en")) {
			customersAddressResponseDTO.setCountryName(customersAddress.getCountry().getNameEnglish());
		} else {
			customersAddressResponseDTO.setCountryName(customersAddress.getCountry().getNameArabic());
		}
		if (customersAddress.getState() != null) {
			customersAddressResponseDTO.setStateId(customersAddress.getState().getId());
			if (locale.getLanguage().equals("en")) {
				customersAddressResponseDTO.setStateName(customersAddress.getState().getNameEnglish());
			} else {
				customersAddressResponseDTO.setStateName(customersAddress.getState().getNameArabic());
			}
		}
		if (customersAddress.getCity() != null) {
			customersAddressResponseDTO.setCityId(customersAddress.getCity().getId());
			if (locale.getLanguage().equals("en")) {
				customersAddressResponseDTO.setCityName(customersAddress.getCity().getNameEnglish());
			} else {
				customersAddressResponseDTO.setCityName(customersAddress.getCity().getNameArabic());
			}
		}
		if (customersAddress.getArea() != null) {
			customersAddressResponseDTO.setAreaId(customersAddress.getArea().getId());
			if (locale.getLanguage().equals("en")) {
				customersAddressResponseDTO.setAreaName(customersAddress.getArea().getNameEnglish());
			} else {
				customersAddressResponseDTO.setAreaName(customersAddress.getArea().getNameArabic());
			}
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