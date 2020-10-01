package com.nice.service;

import java.util.List;
import java.util.Optional;

import org.springframework.stereotype.Component;

import com.nice.dto.DeviceDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.DeviceDetail;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public interface DeviceDetailService {
	/**
	 * add and update combine method for device detail
	 *
	 * @param  deviceDetailDTO
	 * @param  userId
	 * @throws NotFoundException
	 */
	void addUpdateDeviceDetail(DeviceDetailDTO deviceDetailDTO) throws NotFoundException;

	/**
	 * get device detail by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	DeviceDetailDTO getDeviceDetailById(Long id) throws NotFoundException;

	/**
	 * get all device detail list
	 *
	 * @return
	 */
	List<DeviceDetailDTO> getAllDeviceDetailList();

	/**
	 * get device detail list by user
	 *
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	Optional<List<DeviceDetail>> getDeviceDetailListByUserId(Long userId) throws NotFoundException;
}
