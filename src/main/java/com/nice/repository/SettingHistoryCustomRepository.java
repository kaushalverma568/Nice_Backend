package com.nice.repository;

import java.util.Date;
import java.util.List;

import com.nice.model.SettingHistory;

public interface SettingHistoryCustomRepository {


	Long getCountBasedOnParams(String fieldName, Date fromDate, Date toDate);

	List<SettingHistory> getListBasedOnParams(Integer startIndex, Integer pageSize, String fieldName, Date fromDate, Date toDate);
	
}
