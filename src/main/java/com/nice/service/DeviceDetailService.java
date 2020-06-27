package com.nice.service;

import java.util.List;
import java.util.Optional;

import org.springframework.stereotype.Component;

import com.nice.dto.DeviceDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.DeviceDetail;

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
	 * get device detail by user optional
	 *
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	Optional<DeviceDetail> getDeviceDetailByUser(Long userId) throws NotFoundException;

	/**
	 * get all device detail list
	 *
	 * @return
	 */
	List<DeviceDetailDTO> getAllDeviceDetailList();

	/**
	 * check for combination of usertype and device id is unique or not
	 *
	 * @param  deviceDetailDTO
	 * @return
	 */
	Boolean checkDeviceIdAlreadyExist(DeviceDetailDTO deviceDetailDTO);

	/**
	 * get device detail by user
	 *
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	DeviceDetail getDeviceDetailByUserId(Long userId) throws NotFoundException;
}
