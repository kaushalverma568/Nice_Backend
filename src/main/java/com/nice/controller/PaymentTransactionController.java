/**
 *
 */
package com.nice.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.OrderListFilterDto;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.PaymentTransactionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.OrdersService;
import com.nice.service.PaymentTransactionService;
import com.nice.util.PaginationUtil;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
@RequestMapping(path = "/payment/transaction")
@RestController
public class PaymentTransactionController {

	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentTransactionController.class);

	@Autowired
	private PaymentTransactionService paymentTransactionService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrdersService orderService;

	/**
	 * get payment transaction list
	 *
	 * @param accessToken
	 * @param orderListFilterDto
	 * @param pageNumber
	 * @param pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@PostMapping("/list/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getPaymentTransactionList(@RequestHeader("Authorization") final String accessToken,
			@RequestBody final OrderListFilterDto orderListFilterDto, @PathVariable final Integer pageNumber, @PathVariable final Integer pageSize)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Payment Transaction List {}", orderListFilterDto);
		Long totalCount = orderService.getOrderCountBasedOnParams(orderListFilterDto);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<PaymentTransactionDTO> paymentTransactionList = paymentTransactionService.getPaymentTransactionList(orderListFilterDto,
				paginationUtilDto.getStartIndex(), pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("transaction.list.message", null))
				.setData(paymentTransactionList).setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}
}
