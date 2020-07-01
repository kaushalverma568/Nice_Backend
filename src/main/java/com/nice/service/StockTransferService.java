package com.nice.service;

import java.util.List;

import com.nice.dto.StockTransferDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Dec-2019
 */
public interface StockTransferService {

	/**
	 * @param stockTransferDto
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void transferStock(StockTransferDto stockTransferDto) throws NotFoundException, ValidationException;

	/**
	 * @param stockTransferDtoList
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void transferStock(List<StockTransferDto> stockTransferDtoList ) throws NotFoundException, ValidationException;

}
