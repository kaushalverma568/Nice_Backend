package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.SettingsDto;
import com.nice.dto.SettingsListDto;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.SettingsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class SettingsValidator implements Validator {

	private static final Logger LOGGER = LoggerFactory.getLogger(SettingsValidator.class);

	@Autowired
	private SettingsService settingsService;
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return SettingsDto.class.equals(clazz) || SettingsListDto.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof SettingsDto) {

			SettingsDto settingDto = (SettingsDto) target;

			LOGGER.info("Before validating the new Settings Object: {}", settingDto);

			if ((settingDto.getFieldName() != null) && settingsService.isSettingExists(settingDto)) {
				errors.rejectValue("fieldName", "409", messageByLocaleService.getMessage("field.name.not.unique", null));
			}

			LOGGER.info("After validating the new Settings Object: {}, total errors {}", settingDto, errors.getFieldErrorCount());
		}
	}

}
