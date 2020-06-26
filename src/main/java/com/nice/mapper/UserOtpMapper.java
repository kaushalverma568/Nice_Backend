package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.UserOtpDto;
import com.nice.model.UserOtp;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Component
public class UserOtpMapper {

	public UserOtpDto toDto(final UserOtp userOtp) {
		UserOtpDto userOtpDTO = new UserOtpDto();
		BeanUtils.copyProperties(userOtp, userOtpDTO);
		userOtpDTO.setUserLoginId(userOtp.getUserLogin().getId());
		return userOtpDTO;
	}

	public UserOtp toEntity(final UserOtpDto userOtpDTO, final Long userId) {
		UserOtp userOtp = new UserOtp();
		BeanUtils.copyProperties(userOtpDTO, userOtp);
		if (userOtpDTO.getId() == null) {
			userOtp.setCreatedBy(userId);
		}
		userOtp.setUpdatedBy(userId);
		return userOtp;
	}

	public List<UserOtpDto> toDtos(final List<UserOtp> userOtpList) {
		List<UserOtpDto> results = new ArrayList<>();
		for (UserOtp userOtp : userOtpList) {
			results.add(toDto(userOtp));
		}
		return results;
	}
}
