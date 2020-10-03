package com.nice.service.impl;

import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.DeviceDetailDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeviceDetailMapper;
import com.nice.model.DeviceDetail;
import com.nice.model.UserLogin;
import com.nice.repository.DeviceDetailRepository;
import com.nice.service.DeviceDetailService;
import com.nice.service.UserLoginService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Service("deviceDetailService")
@Transactional(rollbackFor = Throwable.class)
public class DeviceDetailServiceImpl implements DeviceDetailService {

	private static final Logger LOGGER = LoggerFactory.getLogger(DeviceDetailServiceImpl.class);

	private static final String USER_NOT_FOUND = "user.not.found";

	@Autowired
	private DeviceDetailMapper deviceDetailMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private DeviceDetailRepository deviceDetailRepository;

	@Autowired
	private UserLoginService userLoginService;

	@Override
	public DeviceDetail addUpdateDeviceDetail(final DeviceDetailDTO deviceDetailDTO) throws NotFoundException {

		Optional<UserLogin> userLogin = userLoginService.getUserLogin(deviceDetailDTO.getUserId());
		if (!userLogin.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage(USER_NOT_FOUND, new Object[] { deviceDetailDTO.getUserId() }));
		}
		/**
		 * Check same deviceId for same user then no need to change anything and return
		 */
		Optional<DeviceDetail> optDeviceDetails = deviceDetailRepository.findByDeviceIdAndUserLogin(deviceDetailDTO.getDeviceId(), userLogin.get());
		if (optDeviceDetails.isPresent()) {
			return optDeviceDetails.get();
		}
		/**
		 * Check same user, and unique id if available then update that device against
		 * that user and unique id and return
		 */
		if (deviceDetailDTO.getUniqueDeviceId() != null) {
			Optional<DeviceDetail> optUniqueDeviceDetails = deviceDetailRepository.findByUniqueDeviceIdAndUserLogin(deviceDetailDTO.getUniqueDeviceId(),
					userLogin.get());
			if (optUniqueDeviceDetails.isPresent()) {
				DeviceDetail deviceDetail = optUniqueDeviceDetails.get();
				deviceDetail.setDeviceId(deviceDetailDTO.getDeviceId());
				deviceDetailRepository.save(deviceDetail);
				return deviceDetail;
			}
		}
		/**
		 * Delete device details by deviceId first and then add new device. This is to
		 * ensure that one device is not associated with multiple users. this is the
		 * case when admin login and vendor login on same browser
		 */
		deleteDeviceDetailByDeviceId(deviceDetailDTO.getDeviceId());

		/**
		 *
		 */

		LOGGER.info("inside update device detail for user {} and deviceid {}", deviceDetailDTO.getUserId(), deviceDetailDTO.getDeviceId());
		DeviceDetail deviceDetail = deviceDetailMapper.toEntity(deviceDetailDTO);

		deviceDetail.setUserLogin(userLogin.get());
		return deviceDetailRepository.save(deviceDetail);
	}

	@Override
	public DeviceDetailDTO getDeviceDetailById(final Long id) throws NotFoundException {
		return deviceDetailMapper.toDto(deviceDetailRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("device.detail.not.found", new Object[] { id }))));
	}

	@Override
	public List<DeviceDetailDTO> getAllDeviceDetailList() {
		return deviceDetailMapper.toDtos(deviceDetailRepository.findAll());
	}

	@Override
	public Optional<List<DeviceDetail>> getDeviceDetailListByUserId(final Long userId) throws NotFoundException {
		Optional<UserLogin> userLogin = userLoginService.getUserLogin(userId);
		if (userLogin.isPresent()) {
			return deviceDetailRepository.findAllByUserLogin(userLogin.get());
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage(USER_NOT_FOUND, new Object[] { userId }));
		}
	}

	private void deleteDeviceDetailByDeviceId(final String deviceId) {
		deviceDetailRepository.deleteByDeviceId(deviceId);
	}

}
