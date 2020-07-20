package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CashCollectionDTO;
import com.nice.model.CashCollection;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public class CashCollectionMapper {
	
	public CashCollectionDTO toDto(final CashCollection cashCollection) {
		CashCollectionDTO cashCollectionResponseDTO = new CashCollectionDTO();
		BeanUtils.copyProperties(cashCollection, cashCollectionResponseDTO);
		cashCollectionResponseDTO.setOrderId(cashCollection.getOrder().getId());
		cashCollectionResponseDTO.setDeliveryboyId(cashCollection.getDeliveryBoy().getId());
		cashCollectionResponseDTO.setDeliveryBoyName(cashCollection.getDeliveryBoy().getFirstName()+ " " + cashCollection.getDeliveryBoy().getLastName());
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
