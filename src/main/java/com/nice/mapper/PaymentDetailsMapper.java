package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.model.PaymentDetails;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 15-07-2020
 */
@Component
public class PaymentDetailsMapper {

	public PaymentDetailsResponseDTO toDto(final PaymentDetails paymentDetails) {
		final Locale locale = LocaleContextHolder.getLocale();
		PaymentDetailsResponseDTO paymentDetailsDTO = new PaymentDetailsResponseDTO();
		BeanUtils.copyProperties(paymentDetails, paymentDetailsDTO);
		if (paymentDetails.getDeliveryBoy() != null) {
			paymentDetailsDTO.setDeliveryBoyId(paymentDetails.getDeliveryBoy().getId());
			if (locale.getLanguage().equals("en")) {
				paymentDetailsDTO
						.setDeliveryBoyName(paymentDetails.getDeliveryBoy().getFirstNameEnglish() + " " + paymentDetails.getDeliveryBoy().getLastNameEnglish());
			} else {
				paymentDetailsDTO
						.setDeliveryBoyName(paymentDetails.getDeliveryBoy().getFirstNameArabic() + " " + paymentDetails.getDeliveryBoy().getLastNameArabic());
			}
		}
		if (paymentDetails.getVendor() != null) {
			paymentDetailsDTO.setVendorId(paymentDetails.getVendor().getId());
			if (locale.getLanguage().equals("en")) {
				paymentDetailsDTO.setVendorName(paymentDetails.getVendor().getFirstNameEnglish() + " " + paymentDetails.getVendor().getLastNameEnglish());
			} else {
				paymentDetailsDTO.setVendorName(paymentDetails.getVendor().getFirstNameArabic() + " " + paymentDetails.getVendor().getLastNameArabic());
			}
		}
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
