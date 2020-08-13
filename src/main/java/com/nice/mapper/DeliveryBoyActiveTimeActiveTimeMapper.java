/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.DeliveryBoyActiveTimeDto;
import com.nice.model.DeliveryBoyActiveTime;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Component
public class DeliveryBoyActiveTimeActiveTimeMapper {

	public DeliveryBoyActiveTimeDto toDto(final DeliveryBoyActiveTime deliveryBoyActiveTime) {

		DeliveryBoyActiveTimeDto deliveryBoyActiveTimeDto = new DeliveryBoyActiveTimeDto();
		BeanUtils.copyProperties(deliveryBoyActiveTime, deliveryBoyActiveTimeDto);
		deliveryBoyActiveTimeDto.setDeliveryBoyId(deliveryBoyActiveTime.getDeliveryBoy().getId());
		return deliveryBoyActiveTimeDto;
	}

	public DeliveryBoyActiveTime toEntity(final DeliveryBoyActiveTimeDto deliveryBoyActiveTimeDTO, final Long userId) {
		DeliveryBoyActiveTime deliveryBoyActiveTime = new DeliveryBoyActiveTime();
		BeanUtils.copyProperties(deliveryBoyActiveTimeDTO, deliveryBoyActiveTime);
		if (userId != null) {
			if (deliveryBoyActiveTimeDTO.getId() == null) {
				deliveryBoyActiveTime.setCreatedBy(userId);
			}
			deliveryBoyActiveTime.setUpdatedBy(userId);
		}
		return deliveryBoyActiveTime;
	}

	public List<DeliveryBoyActiveTimeDto> toDtos(final List<DeliveryBoyActiveTime> deliveryBoyActiveTimeList) {
		List<DeliveryBoyActiveTimeDto> results = new ArrayList<>();
		for (DeliveryBoyActiveTime DeliveryBoyActiveTime : deliveryBoyActiveTimeList) {
			results.add(toDto(DeliveryBoyActiveTime));
		}
		return results;
	}
}
