package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.StockAllocationDto;
import com.nice.model.StockAllocation;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jan-2020
 */
@Component
public class StockAllocationMapper {

	public StockAllocationDto toDto(final StockAllocation stockAllocation) {
		StockAllocationDto stockAllocationDto = new StockAllocationDto();

		BeanUtils.copyProperties(stockAllocation, stockAllocationDto);
		return stockAllocationDto;
	}

	public StockAllocation toEntity(final StockAllocationDto stockAllocationDto, final Long userId) {
		StockAllocation stockAllocation = new StockAllocation();
		BeanUtils.copyProperties(stockAllocationDto, stockAllocation);
		if (stockAllocationDto.getId() == null) {
			stockAllocation.setCreatedBy(userId);
		}
		stockAllocation.setUpdatedBy(userId);
		return stockAllocation;
	}

	public List<StockAllocationDto> toDtos(final List<StockAllocation> stockAllocationList) {
		List<StockAllocationDto> results = new ArrayList<>();
		for (StockAllocation StockAllocation : stockAllocationList) {
			results.add(toDto(StockAllocation));
		}
		return results;
	}
}
