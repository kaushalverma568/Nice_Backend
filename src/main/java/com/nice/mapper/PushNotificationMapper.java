package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.PushNotificationResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.PushNotification;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 28, 2020
 */
@Component
public class PushNotificationMapper {

	public PushNotificationResponseDTO toDto(final PushNotification pushNotification) {
		PushNotificationResponseDTO pushNotificationResponseDTO = new PushNotificationResponseDTO();
		BeanUtils.copyProperties(pushNotification, pushNotificationResponseDTO);
		if ("en".equals(LocaleContextHolder.getLocale().getLanguage())) {
			pushNotificationResponseDTO.setMessage(pushNotification.getMessageEnglish());
		} else {
			pushNotificationResponseDTO.setMessage(pushNotification.getMessageArabic());
		}
		return pushNotificationResponseDTO;
	}

	public List<PushNotificationResponseDTO> toDtos(final List<PushNotification> pushNotifications) throws NotFoundException {
		List<PushNotificationResponseDTO> results = new ArrayList<>();
		for (PushNotification pushNotificationReceiver : pushNotifications) {
			results.add(toDto(pushNotificationReceiver));
		}
		return results;
	}
}
