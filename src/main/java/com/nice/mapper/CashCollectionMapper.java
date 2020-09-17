package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.CashCollectionDTO;
import com.nice.model.CashCollection;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component
public class CashCollectionMapper {

	public CashCollectionDTO toDto(final CashCollection cashCollection) {
		final Locale locale = LocaleContextHolder.getLocale();
		CashCollectionDTO cashCollectionResponseDTO = new CashCollectionDTO();
		BeanUtils.copyProperties(cashCollection, cashCollectionResponseDTO);
		cashCollectionResponseDTO.setOrderId(cashCollection.getOrder().getId());
		cashCollectionResponseDTO.setDeliveryboyId(cashCollection.getDeliveryBoy().getId());
		cashCollectionResponseDTO
				.setDeliveryBoyNameEnglish(cashCollection.getDeliveryBoy().getFirstNameEnglish() + " " + cashCollection.getDeliveryBoy().getLastNameEnglish());
		cashCollectionResponseDTO
				.setDeliveryBoyNameArabic(cashCollection.getDeliveryBoy().getFirstNameArabic() + " " + cashCollection.getDeliveryBoy().getLastNameArabic());
		cashCollectionResponseDTO.setCustomerName(
				cashCollection.getOrder().getCustomer().getFirstName().concat(" ").concat(cashCollection.getOrder().getCustomer().getLastName()));
		if (locale.getLanguage().equals("en")) {
			cashCollectionResponseDTO
					.setDeliveryBoyName(cashCollection.getDeliveryBoy().getFirstNameEnglish() + " " + cashCollection.getDeliveryBoy().getLastNameEnglish());
		} else {
			cashCollectionResponseDTO
					.setDeliveryBoyName(cashCollection.getDeliveryBoy().getFirstNameArabic() + " " + cashCollection.getDeliveryBoy().getLastNameArabic());
		}
		cashCollectionResponseDTO.setTaskId(cashCollection.getTask().getId());
		return cashCollectionResponseDTO;
	}

	public CashCollection toEntity(final CashCollectionDTO cashCollectionDTO) {
		CashCollection cashCollection = new CashCollection();
		BeanUtils.copyProperties(cashCollectionDTO, cashCollection);
		return cashCollection;
	}

	public List<CashCollectionDTO> toDtos(final List<CashCollection> cashCollectionList) {
		List<CashCollectionDTO> results = new ArrayList<>();
		for (CashCollection CashCollection : cashCollectionList) {
			results.add(toDto(CashCollection));
		}
		return results;
	}
}
