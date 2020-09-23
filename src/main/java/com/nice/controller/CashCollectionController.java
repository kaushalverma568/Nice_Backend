package com.nice.controller;

import java.util.Date;
import java.util.List;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.CashCollectionDTO;
import com.nice.dto.PaginationUtilDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CashCollectionMapper;
import com.nice.model.CashCollection;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CashcollectionService;
import com.nice.util.PaginationUtil;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */

@RequestMapping(path = "/cashCollection")
@RestController
public class CashCollectionController {

	private static final Logger LOGGER = LoggerFactory.getLogger(CashCollectionController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CashcollectionService cashCollectionService;

	@Autowired
	private CashCollectionMapper cashCollectionMapper;

	@PostMapping
	public ResponseEntity<Object> addCashCollection(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final CashCollectionDTO cashCollectionDTO) throws NotFoundException, ValidationException {
		LOGGER.info("Inside add CashCollection {}", cashCollectionDTO);
		CashCollectionDTO resultCashCollection = cashCollectionService.addCashCollection(cashCollectionDTO);
		LOGGER.info("Outside add CashCollection {}", resultCashCollection);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cash.collection.create.message", null)).setData(resultCashCollection).create();
	}

	@GetMapping(value = "/{cashCollectionId}")
	public ResponseEntity<Object> getById(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("cashCollectionId") final Long cashCollectionId) throws NotFoundException {
		CashCollectionDTO resultCashCollection = cashCollectionService.getCashCollection(cashCollectionId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cash.collection.detail.message", null)).setData(resultCashCollection).create();
	}

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "deliveryBoyId", required = false) final Long deliveryBoyId,
			@RequestParam(name = "paidDate", required = false) final Date createdAt) throws ValidationException {

		Long totalCount = cashCollectionService.getCountBasedOnParams(deliveryBoyId, createdAt);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<CashCollection> collectionList = cashCollectionService.getListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, deliveryBoyId,
				createdAt);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cash.collection.list.message", null)).setData(cashCollectionMapper.toDtos(collectionList))
				.setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.create();
	}

}