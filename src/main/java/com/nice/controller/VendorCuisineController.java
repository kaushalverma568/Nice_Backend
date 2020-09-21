package com.nice.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.VendorCuisineDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.VendorCuisineService;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 25, 2020
 */
@RequestMapping(path = "/vendor/cuisine")
@RestController
public class VendorCuisineController {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(VendorCuisineController.class);
	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private VendorCuisineService vendorCuisineService;

	/**
	 * fetch vendor cuisine list
	 *
	 * @param accessToken
	 * @param vendorId
	 * @param active
	 * @return
	 */
	@GetMapping("/list/{vendorId}")
	public ResponseEntity<Object> getVendorCuisineList(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@RequestParam(name = "active", required = false) final Boolean active) {
		LOGGER.info("Inside get Vendor cuisine list for id:{}", vendorId);
		final List<VendorCuisineDTO> vendorBasicDetailDTO = vendorCuisineService.getVendorCuisineDetailListByVendor(vendorId, active);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.cuisine.list.message", null))
				.setData(vendorBasicDetailDTO).create();
	}
}
