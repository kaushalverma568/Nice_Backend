package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.StockTransferDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.mapper.StockTransferMapper;
import com.nice.model.StockTransfer;
import com.nice.model.ProductVariant;
import com.nice.model.StockDetails;
import com.nice.repository.StockTransferRepository;
import com.nice.service.StockTransferService;
import com.nice.service.StockDetailsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 31-Jan-2020
 */
@Service("stockTransferService")
@Transactional(rollbackFor = Throwable.class)
public class StockTransferServiceImpl implements StockTransferService {

	@Autowired
	private StockTransferRepository stockTransferRepository;

	@Autowired
	private StockDetailsService stockDetailsService;


	@Autowired
	private StockTransferMapper stockTransferMapper;
	private static final Logger LOGGER = LoggerFactory.getLogger(StockTransferServiceImpl.class);

	@Override
	public void transferStock(final StockTransferDto stockTransferDto) throws NotFoundException, ValidationException {
		LOGGER.info("Internal Stock Transfer with details:{}", stockTransferDto);
		/**
		 * Add the transaction in the internal stock transfer table
		 */
		StockTransfer stockTransfer = stockTransferMapper.toEntity(stockTransferDto);
		ProductVariant productVariant =stockDetailsService.getproductVariantFromProductIdAndUomId(stockTransferDto.getProductId(), stockTransferDto.getUomId());
		StockDetails stockDetails = stockDetailsService.getStockDetailsForProductVarientAndLot(productVariant, stockTransferDto.getVendorId(), stockTransferDto.getLotNo());
		stockTransfer.setStockDetails(stockDetails);
		stockTransfer.setVendorId(stockTransferDto.getVendorId());
		stockTransfer.setIsManual(Boolean.valueOf(false));
		stockTransferRepository.save(stockTransfer);
		/**
		 * Update the qty of all the status based on the stock transfer
		 */
		LOGGER.info("Internal Stock Transfer entry done in table, now updating the qty based on transfer");
		stockDetailsService.updateStockDetails(stockTransferDto);
	}

	@Override
	public void transferStock(final List<StockTransferDto> stockTransferDtoList) throws NotFoundException, ValidationException {
		LOGGER.info("Internal Stock Transfer List");
		for (StockTransferDto stockTransferDto : stockTransferDtoList) {
			transferStock(stockTransferDto);
		}
		LOGGER.info("Internal Stock Transfer list entry done");
	}




}
