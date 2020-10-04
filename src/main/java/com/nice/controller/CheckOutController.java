/**
 *
 */
package com.nice.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.CheckOutDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CheckOutService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Sep-2020
 */
@RequestMapping(path = "/checkout")
@RestController
public class CheckOutController {

	@Autowired
	private CheckOutService checkOutService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@GetMapping()
	public ResponseEntity<Object> checkout(@RequestHeader("Authorization") final String accessToken, @RequestParam(required = true) final String deliveryType,
			@RequestParam(required = true) final Boolean useWallet, @RequestParam(required = false) final Long shippingAddressId)
			throws ValidationException, NotFoundException {

		CheckOutDTO checkOutDto = checkOutService.getCheckOutPageDetails(deliveryType, useWallet, shippingAddressId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(checkOutDto)
				.setMessage(messageByLocaleService.getMessage("check.out.details.displayed", null)).create();
	}
}
