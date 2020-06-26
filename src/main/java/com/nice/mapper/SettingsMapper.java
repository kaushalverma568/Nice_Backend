package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.SettingsDto;
import com.nice.model.Settings;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class SettingsMapper {

	public SettingsDto toDto(final Settings settings) {
		SettingsDto settingsDto = new SettingsDto();
		BeanUtils.copyProperties(settings, settingsDto);
		return settingsDto;
	}

	public Settings toEntity(final SettingsDto settingsDTO) {
		Settings settings = new Settings();
		BeanUtils.copyProperties(settingsDTO, settings);
		return settings;
	}

	public List<SettingsDto> toDtos(final List<Settings> settingsList) {
		List<SettingsDto> results = new ArrayList<>();
		for (Settings settings : settingsList) {
			results.add(toDto(settings));
		}
		return results;
	}
}
