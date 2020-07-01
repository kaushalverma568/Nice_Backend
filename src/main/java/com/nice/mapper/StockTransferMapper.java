package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.StockTransferDto;
import com.nice.model.StockTransfer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jan-2020
 */
@Component
public class StockTransferMapper {

	public StockTransferDto toDto(final StockTransfer stockTransfer) {
		StockTransferDto stockTransferDto = new StockTransferDto();
		BeanUtils.copyProperties(stockTransfer, stockTransferDto);
		stockTransferDto.setLotNo(stockTransfer.getStockDetails().getLotNo());
		stockTransferDto.setVendorId(stockTransfer.getStockDetails().getVendorId());
		return stockTransferDto;
	}

	public StockTransfer toEntity(final StockTransferDto stockTransferDto) {
		StockTransfer stockTransfer = new StockTransfer();
		BeanUtils.copyProperties(stockTransferDto, stockTransfer);
		stockTransfer.setActive(true);
		return stockTransfer;
	}

	public List<StockTransferDto> toDtos(final List<StockTransfer> stockTransfer) {
		List<StockTransferDto> results = new ArrayList<>();
		for (StockTransfer c : stockTransfer) {
			results.add(toDto(c));
		}
		return results;
	}
}
