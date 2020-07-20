package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.model.PaymentDetails;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 15-07-2020
 */
@Component
public class PaymentDetailsMapper {

	public PaymentDetailsResponseDTO toDto(final PaymentDetails paymentDetails) {
		PaymentDetailsResponseDTO paymentDetailsDTO = new PaymentDetailsResponseDTO();
		BeanUtils.copyProperties(paymentDetails, paymentDetailsDTO);
		return paymentDetailsDTO;
	}

	public PaymentDetails toEntity(final PaymentDetailsDTO paymentDetailsDTO) {
		PaymentDetails paymentDetails = new PaymentDetails();
		BeanUtils.copyProperties(paymentDetailsDTO, paymentDetails);
		return paymentDetails;
	}

	public List<PaymentDetailsResponseDTO> toDtos(final List<PaymentDetails> paymentDetailsList) {
		List<PaymentDetailsResponseDTO> results = new ArrayList<>();
		for (PaymentDetails paymentDetails : paymentDetailsList) {
			results.add(toDto(paymentDetails));
		}
		return results;
	}
}
