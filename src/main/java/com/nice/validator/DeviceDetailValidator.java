package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.DeviceDetailDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.DeviceDetailService;

@Component
public class DeviceDetailValidator implements Validator {
	private static final Logger LOGGER = LoggerFactory.getLogger(DeviceDetailValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private DeviceDetailService deviceDetailService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return DeviceDetailDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final DeviceDetailDTO deviceDetailDTO = (DeviceDetailDTO) target;

		/**
		 * Check device is exists or not
		 */
		if (deviceDetailDTO != null && deviceDetailService.checkDeviceIdAlreadyExist(deviceDetailDTO)) {
			LOGGER.info("inside email chcek");
			errors.rejectValue("deviceId", "409", messageByLocaleService.getMessage("device.user.exist", null));
		}
	}
}
