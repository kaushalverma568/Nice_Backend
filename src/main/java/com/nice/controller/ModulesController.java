package com.nice.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ModulesMapper;
import com.nice.model.Modules;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ModulesService;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@RequestMapping(path = "/module")
@RestController
public class ModulesController {

	private static final Logger LOGGER = LoggerFactory.getLogger(ModulesController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	/**
	 * service - to implement business logic
	 */
	@Autowired
	private ModulesService modulesService;

	@Autowired
	private ModulesMapper modulesMapper;

	/**
	 * get module list
	 *
	 * @param  accessToken
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Role-Permission','CAN_VIEW')")
	public ResponseEntity<Object> getModuleList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "availableForNewRole", required = false) final Boolean availableForNewRole) {
		LOGGER.info("Inside get module list activeRecords:{} ,availableForNewRole:{}", activeRecords, availableForNewRole);
		final Page<Modules> resultRes = modulesService.getModuleList(pageNumber, pageSize, activeRecords, availableForNewRole);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("modules.list.message", null))
				.setData(modulesMapper.toDTOs(resultRes.getContent())).setHasNextPage(resultRes.hasNext()).setHasPreviousPage(resultRes.hasPrevious())
				.setTotalPages(resultRes.getTotalPages()).setPageNumber(resultRes.getNumber() + 1).setTotalCount(resultRes.getTotalElements()).create();

	}
}
