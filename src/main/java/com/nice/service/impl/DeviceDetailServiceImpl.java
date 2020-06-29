package com.nice.service.impl;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
@Transactional(rollbackOn = Throwable.class)
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
	public void addUpdateDeviceDetail(final DeviceDetailDTO deviceDetailDTO) throws NotFoundException {
		LOGGER.info("inside add update device detail method for data {}", deviceDetailDTO);
		Optional<DeviceDetail> existingDetail = getDeviceDetailByUser(deviceDetailDTO.getUserId());
		if (existingDetail.isPresent()) {
			if (!existingDetail.get().getDeviceId().equals(deviceDetailDTO.getDeviceId())) {
				LOGGER.info("inside update device detail for id {}", existingDetail.get().getId());
				existingDetail.get().setDeviceId(deviceDetailDTO.getDeviceId());
				existingDetail.get().setDeviceType(deviceDetailDTO.getDeviceType());
				deviceDetailRepository.save(existingDetail.get());
			}
		} else {
			LOGGER.info("inside update device detail for user {} and deviceid {}", deviceDetailDTO.getUserId(), deviceDetailDTO.getDeviceId());
			DeviceDetail deviceDetail = deviceDetailMapper.toEntity(deviceDetailDTO);
			Optional<UserLogin> userLogin = userLoginService.getUserLogin(deviceDetailDTO.getUserId());
			if (userLogin.isPresent()) {
				deviceDetail.setUserLogin(userLogin.get());
			} else {
				throw new NotFoundException(messageByLocaleService.getMessage(USER_NOT_FOUND, new Object[] { deviceDetailDTO.getUserId() }));
			}
			deviceDetailRepository.save(deviceDetail);
		}
	}

	@Override
	public DeviceDetailDTO getDeviceDetailById(final Long id) throws NotFoundException {
		return deviceDetailMapper.toDto(deviceDetailRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("device.detail.not.found", new Object[] { id }))));
	}

	@Override
	public Optional<DeviceDetail> getDeviceDetailByUser(final Long userId) throws NotFoundException {
		Optional<UserLogin> userLogin = userLoginService.getUserLogin(userId);
		if (userLogin.isPresent()) {
			return deviceDetailRepository.findByUserLogin(userLogin.get());
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage(USER_NOT_FOUND, new Object[] { userId }));
		}
	}

	@Override
	public List<DeviceDetailDTO> getAllDeviceDetailList() {
		return deviceDetailMapper.toDtos(deviceDetailRepository.findAll());
	}

	@Override
	public Boolean checkDeviceIdAlreadyExist(final DeviceDetailDTO deviceDetailDTO) {
		if (deviceDetailDTO.getId() != null) {
			return deviceDetailRepository
					.findByDeviceIdAndUserTypeAndIdNot(deviceDetailDTO.getDeviceId(), deviceDetailDTO.getUserType(), deviceDetailDTO.getId()).isPresent();
		} else {
			return deviceDetailRepository.findByDeviceIdAndUserType(deviceDetailDTO.getDeviceId(), deviceDetailDTO.getUserType()).isPresent();
		}
	}

	@Override
	public DeviceDetail getDeviceDetailByUserId(final Long userId) throws NotFoundException {
		Optional<UserLogin> userLogin = userLoginService.getUserLogin(userId);
		if (userLogin.isPresent()) {
			return deviceDetailRepository.findByUserLogin(userLogin.get())
					.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("device.detail.user.not.found", new Object[] { userId })));
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage(USER_NOT_FOUND, new Object[] { userId }));
		}
	}

}
