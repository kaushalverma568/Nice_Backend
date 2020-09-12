package com.nice.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.UserType;
import com.nice.dto.UsersResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.SettingHistory;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.SettingHistoryRepository;
import com.nice.service.SettingHistoryService;
import com.nice.service.UserLoginService;
import com.nice.service.UsersService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class SettingHistoryServiceImpl implements SettingHistoryService {

	@Autowired
	private SettingHistoryRepository settingHistoryRepository;

	@Autowired
	private UserLoginService userLoginSerive;

	@Autowired
	private VendorService vendorSerive;

	@Autowired
	private UsersService usersSerive;

	@Override
	public Long getCountBasedOnParams(final String fieldName, final Date fromDate, final Date toDate) {
		return settingHistoryRepository.getCountBasedOnParams(fieldName, fromDate, toDate);
	}

	@Override
	public List<SettingHistory> getListBasedOnParams(final Integer startIndex, final Integer pageSize, final String fieldName, final Date fromDate,
			final Date toDate) throws NotFoundException, ValidationException {
		List<SettingHistory> settingList = settingHistoryRepository.getListBasedOnParams(startIndex, pageSize, fieldName, fromDate, toDate);
		Locale locale = LocaleContextHolder.getLocale();
		for (SettingHistory settingHistory : settingList) {
			UserLogin userLogin = userLoginSerive.getUserLoginDetail(settingHistory.getUpdatedBy());
			if (userLogin.getEntityType() == null) {
				settingHistory.setUserName("Super Admin");
			} else if (userLogin.getEntityType().equals(UserType.VENDOR.name())) {
				Vendor vendor = vendorSerive.getVendorDetail(userLogin.getEntityId());
				settingHistory.setUserName(locale.getLanguage().equals("en") ? vendor.getFirstNameEnglish() + " " + vendor.getLastNameEnglish()
						: vendor.getFirstNameArabic() + " " + vendor.getLastNameArabic());
			} else if (userLogin.getEntityType().equals(UserType.USER.name())) {
				UsersResponseDTO user = usersSerive.getUsers(userLogin.getEntityId());
				settingHistory.setUserName(locale.getLanguage().equals("en") ? user.getFirstNameEnglish() + " " + user.getLastNameEnglish()
						: user.getFirstNameArabic() + " " + user.getLastNameArabic());
			}
		}
		return settingList;
	}

}
