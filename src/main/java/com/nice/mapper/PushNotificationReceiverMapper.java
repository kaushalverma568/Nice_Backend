package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.PushNotificationResponseDTO;
import com.nice.model.PushNotificationReceiver;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 28, 2020
 */
@Component
public class PushNotificationReceiverMapper {

	public PushNotificationResponseDTO toDto(final PushNotificationReceiver pushNotificationReceiver) {
		PushNotificationResponseDTO pushNotificationResponseDTO = new PushNotificationResponseDTO();
		BeanUtils.copyProperties(pushNotificationReceiver, pushNotificationResponseDTO);
		if ("en".equals(LocaleContextHolder.getLocale().getLanguage())) {
			pushNotificationResponseDTO.setMessage(pushNotificationReceiver.getMessageEnglish());
		} else {
			pushNotificationResponseDTO.setMessage(pushNotificationReceiver.getMessageArabic());
		}
		return pushNotificationResponseDTO;
	}

	public List<PushNotificationResponseDTO> toDtos(final List<PushNotificationReceiver> pushNotificationReceivers) {
		List<PushNotificationResponseDTO> results = new ArrayList<>();
		for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
			results.add(toDto(pushNotificationReceiver));
		}
		return results;
	}
}
