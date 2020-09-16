package com.nice.service;

import java.util.List;

import com.nice.dto.OrderListFilterDto;
import com.nice.dto.PaymentTransactionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
public interface PaymentTransactionService {

	/**
	 * get payment transaction list
	 *
	 * @param orderListFilterDto
	 * @param startIndex
	 * @param pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<PaymentTransactionDTO> getPaymentTransactionList(OrderListFilterDto orderListFilterDto, Integer startIndex, Integer pageSize)
			throws NotFoundException, ValidationException;

}
