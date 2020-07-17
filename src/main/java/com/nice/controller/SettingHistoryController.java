package com.nice.controller;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.PaginationUtilDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.SettingHistory;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.SettingHistoryService;
import com.nice.util.PaginationUtil;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@RequestMapping(path = "/setting/history")
@RestController
public class SettingHistoryController {


	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private SettingHistoryService settingHistoryService;
	

	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getList(@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "fromDate", required = false) final Date fromDate,
			@RequestParam(name = "toDate", required = false) final Date toDate,
			@RequestParam(name = "fieldName", required = false) final String fieldName) throws ValidationException, NotFoundException {
		
		Long totalCount = settingHistoryService.getCountBasedOnParams(fieldName,fromDate,toDate);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<SettingHistory> resultSettingHistory = settingHistoryService.getListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, fieldName,fromDate,toDate);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("setting.history.list.message", null)).setData(resultSettingHistory)
				.setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.create();
	}

}