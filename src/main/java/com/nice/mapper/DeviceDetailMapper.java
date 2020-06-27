package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.DeviceDetailDTO;
import com.nice.model.DeviceDetail;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 08-Jan-2020
 */
@Component
public class DeviceDetailMapper {

	public DeviceDetailDTO toDto(final DeviceDetail deviceDetail) {
		DeviceDetailDTO deviceDetailDTO = new DeviceDetailDTO();
		BeanUtils.copyProperties(deviceDetail, deviceDetailDTO);
		return deviceDetailDTO;
	}

	public DeviceDetail toEntity(final DeviceDetailDTO deviceDetailDTO) {
		DeviceDetail deviceDetail = new DeviceDetail();
		BeanUtils.copyProperties(deviceDetailDTO, deviceDetail);
		return deviceDetail;
	}

	public List<DeviceDetailDTO> toDtos(final List<DeviceDetail> deviceDetailList) {
		List<DeviceDetailDTO> results = new ArrayList<>();
		for (DeviceDetail deviceDetail : deviceDetailList) {
			results.add(toDto(deviceDetail));
		}
		return results;
	}
}
