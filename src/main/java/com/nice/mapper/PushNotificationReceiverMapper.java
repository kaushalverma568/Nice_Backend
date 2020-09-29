package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.PushNotificationResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.PushNotificationReceiver;
import com.nice.service.PushNotificationService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 28, 2020
 */
@Component
public class PushNotificationReceiverMapper {

	@Autowired
	PushNotificationService pushNotificationService;

	public PushNotificationResponseDTO toDto(final PushNotificationReceiver pushNotificationReceiver) throws NotFoundException {
		PushNotificationResponseDTO pushNotificationResponseDTO = new PushNotificationResponseDTO();
		BeanUtils.copyProperties(pushNotificationReceiver, pushNotificationResponseDTO);
		pushNotificationResponseDTO.setModule(pushNotificationService.getPushNotificationDetail(pushNotificationReceiver.getId()).getModule());
		if ("en".equals(LocaleContextHolder.getLocale().getLanguage())) {
			pushNotificationResponseDTO.setMessage(pushNotificationReceiver.getMessageEnglish());
		} else {
			pushNotificationResponseDTO.setMessage(pushNotificationReceiver.getMessageArabic());
		}
		return pushNotificationResponseDTO;
	}

	public List<PushNotificationResponseDTO> toDtos(final List<PushNotificationReceiver> pushNotificationReceivers) throws NotFoundException {
		List<PushNotificationResponseDTO> results = new ArrayList<>();
		for (PushNotificationReceiver pushNotificationReceiver : pushNotificationReceivers) {
			results.add(toDto(pushNotificationReceiver));
		}
		return results;
	}
}
