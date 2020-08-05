package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.SettingsConstant;
import com.nice.dto.SettingsDto;
import com.nice.dto.SettingsListDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SettingsMapper;
import com.nice.model.SettingHistory;
import com.nice.model.Settings;
import com.nice.model.UserLogin;
import com.nice.repository.SettingHistoryRepository;
import com.nice.repository.SettingsRepository;
import com.nice.service.SettingsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service(value = "settingsService")
@Transactional(rollbackFor = Throwable.class)
public class SettingsServiceImpl implements SettingsService {

	private static final Logger LOGGER = LoggerFactory.getLogger(SettingsServiceImpl.class);

	private static final String SETTING_NOT_FOUND = "settings.not.found.name";

	@Autowired
	private SettingsRepository settingsRepository;

	@Autowired
	private SettingHistoryRepository settingsHistoryRepository;
	
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private SettingsMapper settingsMapper;

	@Override
	public SettingsDto addSettings(SettingsDto settingsDto) throws ValidationException {
		LOGGER.info("Inside add Settings method, {}", settingsDto);
		Settings settings = settingsMapper.toEntity(settingsDto);
		UserLogin user = checkForUserLogin();
		settings.setCreatedBy(user.getId());
		settings.setUpdatedBy(user.getId());
		settings.setId(settingsRepository.addSettingsParameters(settings));
		settingsDto = settingsMapper.toDto(settings);
		return settingsDto;
	}

	@Override
	public int updateSettings(final SettingsDto settingsDto) throws NotFoundException, ValidationException {
		/**
		 * Check if settings for given Id exists
		 */
		String previousFieldValue = getSettings(settingsDto.getId()).getFieldValue();
		LOGGER.info("Inside update Settings method, {}", settingsDto);
		Settings settings = settingsMapper.toEntity(settingsDto);
		UserLogin user = checkForUserLogin();
		settings.setUpdatedBy(user.getId());
		int updateCount = settingsRepository.updateSettingsParameters(settings);
		if (updateCount > 0) {
			SettingsConstant.setSettingsValue(settingsDto.getFieldName(), settingsDto.getFieldValue());
		}
		if (!settings.getEncrypted() && !previousFieldValue.equals(settings.getFieldValue())) {
			SettingHistory settingHistory = new SettingHistory();
			settingHistory.setActive(true);
			settingHistory.setCreatedAt(settings.getUpdatedAt());
			settingHistory.setUpdatedAt(settings.getUpdatedAt());
			settingHistory.setCreatedBy(settings.getUpdatedBy());
			settingHistory.setUpdatedBy(settings.getUpdatedBy());
			settingHistory.setCurrentFieldValue(settings.getFieldValue());
			settingHistory.setFieldName(settings.getFieldName());
			settingHistory.setPreviousFieldValue(previousFieldValue);
			settingsHistoryRepository.save(settingHistory);
		}
		
		return updateCount;
	}

	@Override
	public List<SettingsDto> getAllSettingsList() {
		LOGGER.info("Inside get Settings List method");
		List<Settings> settingsList = settingsRepository.findAll();
		List<SettingsDto> settingsDtoList = new ArrayList<>();
		for (Settings settings : settingsList) {
			settingsDtoList.add(settingsMapper.toDto(settings));
		}
		return settingsDtoList;
	}

	@Override
	public SettingsDto getSettingsDetailsById(final Long id) throws ValidationException {
		LOGGER.info("Inside get Settings method, for id: {}", id);
		Settings settings = settingsRepository.findById(id)
				.orElseThrow(() -> new ValidationException(messageByLocaleService.getMessage("settings.not.found", new Object[] { id })));
		return settingsMapper.toDto(settings);
	}

	@Override
	public SettingsDto getSettingsDetailsByFieldName(final String fieldName) throws ValidationException {
		LOGGER.info("Inside get Settings method, for fieldName: {}", fieldName);
		Settings settings = settingsRepository.findByFieldNameIgnoreCase(fieldName)
				.orElseThrow(() -> new ValidationException(messageByLocaleService.getMessage(SETTING_NOT_FOUND, new Object[] { fieldName })));
		return settingsMapper.toDto(settings);
	}

	@Override
	public SettingsDto getSettingsDetailsByNameForEncryptedFields(final String fieldName) throws NotFoundException, ValidationException {
		SettingsDto settingsDto = new SettingsDto();
		LOGGER.info("Inside get encrypted Settings method, for field: {}", fieldName);
		/**
		 * Check if the passed parameter actually contains encrypted settings
		 */
		Optional<Settings> existingSettings = settingsRepository.findByFieldNameIgnoreCase(fieldName);
		if (!existingSettings.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage(SETTING_NOT_FOUND, new Object[] { fieldName }));
		}
		/**
		 * If the return settings field is not encrypted throw exception, this is because wrong parameters have been passed to
		 * the service.
		 */
		if (Boolean.FALSE.equals(existingSettings.get().getEncrypted())) {
			LOGGER.error("Before Throwing Exception : The object returned from DB: {}", existingSettings);
			throw new ValidationException(messageByLocaleService.getMessage("field.not.encrypted", new Object[] { fieldName }));
		}

		/**
		 * After ensuring that the field is encrypted, fetch the decrypted value from the database for the encrypted field.
		 */
		Settings settings = settingsRepository.getDecryptedEntityValue(fieldName);
		BeanUtils.copyProperties(settings, settingsDto);
		return settingsDto;
	}

	@Override
	public SettingsDto getSettingsDetailsByNameForNonEncryptedFields(final String fieldName) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get non encrypted Settings method, for field: {}", fieldName);
		Optional<Settings> settings = settingsRepository.findByFieldNameIgnoreCase(fieldName);
		if (!settings.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage(SETTING_NOT_FOUND, new Object[] { fieldName }));
		}
		/**
		 * If the return settings field is not encrypted throw exception, this is because wrong parameters have been passed to
		 * the service.
		 */
		if (Boolean.TRUE.equals(settings.get().getEncrypted())) {
			LOGGER.error("Before Throwing Exception : The object returned from DB: {}", settings);
			throw new ValidationException(messageByLocaleService.getMessage("field.is.encrypted", new Object[] { fieldName }));
		}
		return settingsMapper.toDto(settings.get());
	}

	@Override
	public boolean isSettingExists(final SettingsDto settingsDto) {
		LOGGER.info("Inside Settings Exists method : {}", settingsDto);
		/**
		 * Case for updation of Settings
		 */
		if (settingsDto.getId() != null) {
			return settingsRepository.findByFieldNameIgnoreCaseAndIdNot(settingsDto.getFieldName(), settingsDto.getId()).isPresent();
		}
		/**
		 * Case for inserting new Settings
		 */
		else {
			return settingsRepository.findByFieldNameIgnoreCase(settingsDto.getFieldName()).isPresent();
		}

	}

	private Settings getSettings(final Long id) throws NotFoundException {
		return settingsRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("settings.not.found", new Object[] { id })));
	}

	
	
	private UserLogin getUserLoginFromToken() {
		Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		if (Constant.ANONYMOUS_USER.equals(principal)) {
			return null;
		}
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	private UserLogin checkForUserLogin() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (userLogin == null) {
			throw new ValidationException(messageByLocaleService.getMessage("login.first", null));
		} else {
			return userLogin;
		}
	}

		
	@Override
	public void updateSettingsList(SettingsListDto settingsListDto) throws NotFoundException, ValidationException {
		for (SettingsDto settingsDto : settingsListDto.getSettingDtoList()) {
			if (settingsDto.getId()!= null) {
				updateSettings(settingsDto);
			} else {
				addSettings(settingsDto);
			}
		}
		
	}

	@Override
	public Map<String, SettingsDto> getSettingsMap() {
		LOGGER.info("Inside get Settings List method");
		List<Settings> settingsList = settingsRepository.findAll();
		Map<String, SettingsDto> settingMap = new HashMap<>();
		for (Settings settings : settingsList) {
			settingMap.put(settings.getFieldName(), settingsMapper.toDto(settings));
		}
		return settingMap;
	}


}
