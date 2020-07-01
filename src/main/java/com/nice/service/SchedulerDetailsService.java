package com.nice.service;

import java.util.List;

import com.nice.dto.SchedulerDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.SchedulerDetails;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
public interface SchedulerDetailsService {

	/**
	 * @param  name
	 * @throws NotFoundException
	 */
	void updateSchedulerDate(String name) throws NotFoundException;

	/**
	 * @return
	 */
	List<SchedulerDetailDTO> getSchedulerList();

	/**
	 * @param  name
	 * @return
	 * @throws NotFoundException
	 */
	SchedulerDetails getLastSchedulerRunDate(String name) throws NotFoundException;

}
