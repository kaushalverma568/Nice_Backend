package com.nice.service;

import java.util.List;
import java.util.Map;

import com.nice.dto.SettingsDto;
import com.nice.dto.SettingsListDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface SettingsService {

	SettingsDto addSettings(SettingsDto settingsDto) throws ValidationException;

	int updateSettings(final SettingsDto settingsDto) throws NotFoundException, ValidationException;

	List<SettingsDto> getAllSettingsList();

	SettingsDto getSettingsDetailsById(final Long id) throws ValidationException;

	/**
	 * Getting setting details for encrypted fields in decrypted format for comparing it with the user input
	 */
	SettingsDto getSettingsDetailsByNameForEncryptedFields(final String fieldName) throws NotFoundException, ValidationException;

	/**
	 * Getting setting details for non encrypted fields
	 */
	SettingsDto getSettingsDetailsByNameForNonEncryptedFields(final String fieldName) throws NotFoundException, ValidationException;

	/**
	 * @param  settingDto
	 * @return
	 */
	boolean isSettingExists(SettingsDto settingDto);

	/**
	 * @param  id
	 * @return
	 * @throws ValidationException
	 */
	SettingsDto getSettingsDetailsByFieldName(String fieldName) throws ValidationException;

	/**
	 * 
	 * @param settingsDtoList
	 * @return
	 * @throws NotFoundException 
	 * @throws ValidationException 
	 */
	void updateSettingsList(SettingsListDto settingsDtoList) throws NotFoundException, ValidationException;

	/**
	 * 
	 * @return
	 */
	Map<String, SettingsDto> getSettingsMap();
}
