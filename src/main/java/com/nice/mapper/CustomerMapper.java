package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CustomerDTO;
import com.nice.dto.CustomerResponseDTO;
import com.nice.model.Customer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Component
public class CustomerMapper {

	public CustomerResponseDTO toDto(final Customer customer) {
		CustomerResponseDTO customerResponseDTO = new CustomerResponseDTO();
		BeanUtils.copyProperties(customer, customerResponseDTO);
		return customerResponseDTO;
	}

	public Customer toEntity(final CustomerDTO customersDto, final Long userId) {
		Customer customers = new Customer();
		BeanUtils.copyProperties(customersDto, customers);
		if (customersDto.getEmail() != null) {
			customers.setEmail(customersDto.getEmail().toLowerCase());
		}
		if (userId != null) {
			if (customersDto.getId() == null) {
				customers.setCreatedBy(userId);
			}
			customers.setUpdatedBy(userId);
		}
		return customers;
	}

	public List<CustomerResponseDTO> toDtos(final List<Customer> customerList) {
		List<CustomerResponseDTO> results = new ArrayList<>();
		for (Customer customer : customerList) {
			results.add(toDto(customer));
		}
		return results;
	}
}