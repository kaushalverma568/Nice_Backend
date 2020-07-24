package com.nice.service;

import java.util.Date;
import java.util.List;

import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.SettingHistory;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface SettingHistoryService {

	Long getCountBasedOnParams(String fieldName, Date fromDate, Date toDate);

	List<SettingHistory> getListBasedOnParams(Integer startIndex, Integer pageSize, String fieldName, Date fromDate, Date toDate)
			throws NotFoundException, ValidationException;

}
