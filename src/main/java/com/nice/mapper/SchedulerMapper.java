package com.nice.mapper;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.SchedulerDetailDTO;
import com.nice.model.SchedulerDetails;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Component
public class SchedulerMapper {

	public SchedulerDetailDTO toDto(final SchedulerDetails schedulerDetail) {
		SchedulerDetailDTO schedulerDetailDTO = new SchedulerDetailDTO();
		BeanUtils.copyProperties(schedulerDetail, schedulerDetailDTO);
		/**
		 * if scheduler is runned for today then set is runned to true
		 */
		schedulerDetailDTO.setIsRunned(CommonUtility.convetUtilDatetoLocalDate(schedulerDetail.getUpdatedAt()).compareTo(LocalDate.now()) >= 0);
		return schedulerDetailDTO;
	}

	public SchedulerDetails toEntity(final SchedulerDetailDTO schedulerDetailDTO, final Long userId) {
		SchedulerDetails schedulerDetail = new SchedulerDetails();
		BeanUtils.copyProperties(schedulerDetailDTO, schedulerDetail);
		return schedulerDetail;
	}

	public List<SchedulerDetailDTO> toDtos(final List<SchedulerDetails> schedulerDetails) {
		List<SchedulerDetailDTO> results = new ArrayList<>();
		for (SchedulerDetails schedulerDetail : schedulerDetails) {
			results.add(toDto(schedulerDetail));
		}
		return results;
	}
}
