package com.nice.controller;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.WalletTrxDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.WalletTrxMapper;
import com.nice.model.WalletTrx;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.WalletTrxService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@RequestMapping(path = "/wallet")
@RestController
public class WalletController {

	private static final Logger LOGGER = LoggerFactory.getLogger(WalletController.class);

	@Autowired
	private WalletTrxService walletTrxService;

	@Autowired
	private WalletTrxMapper walletTrxMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@PostMapping
	public ResponseEntity<Object> addWalletTrx(@RequestBody @Valid final WalletTrxDTO walletTrxDTO) throws NotFoundException {
		LOGGER.info("Inside add walletTrx {}", walletTrxDTO);
		walletTrxService.addupdateWalletTrx(walletTrxDTO);
		LOGGER.info("Outside add walletTrx ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(null).create();
	}

	/**
	 * Get WalletTrx list based on parameters
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param stateId
	 * @param searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getWalletTrxList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "customerId", required = false) final Long customerId) throws NotFoundException {
		final Page<WalletTrx> walletTransactions = walletTrxService.getWalletTrxList(pageNumber, pageSize, customerId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("walletTrx.list.message", null))
				.setData(walletTrxMapper.toDtos(walletTransactions.getContent())).setHasNextPage(walletTransactions.hasNext())
				.setHasPreviousPage(walletTransactions.hasPrevious()).setTotalPages(walletTransactions.getTotalPages()).setPageNumber(walletTransactions.getNumber() + 1)
				.setTotalCount(walletTransactions.getTotalElements()).create();

	}

}
